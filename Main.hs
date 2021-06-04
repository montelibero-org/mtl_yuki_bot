{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main, CacheId) where

import           Control.Applicative ((<|>))
import           Control.Lens (view)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.Map.Strict (Map, (!?))
import           Data.Ratio ((%))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time (UTCTime, addUTCTime, getCurrentTime,
                            secondsToNominalDiffTime)
import qualified Data.Yaml as Yaml
import qualified Database.Persist as Persist
import           Database.Persist.Sqlite (runMigration, runSqlite)
import           Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase,
                                      share, sqlSettings)
import           GHC.Generics (Generic)
import qualified Network.Wreq as Wreq
import           Telegram.Bot.API (ParseMode (Markdown), Update,
                                   defaultTelegramClientEnv)
import           Telegram.Bot.Simple (BotApp (..), BotM, Eff, eff, getEnvToken,
                                      reply, replyMessageParseMode, replyText,
                                      startBot_, toReplyMessage, (<#))
import           Telegram.Bot.Simple.Debug (traceBotDefault)
import           Telegram.Bot.Simple.UpdateParser (parseUpdate)
import qualified Telegram.Bot.Simple.UpdateParser as UpdateParser
import           Text.Printf (printf)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
    Cache
      Id            Text
      responseBody  ByteString
      updated       UTCTime
  |]

main :: IO ()
main = do
  token     <- getEnvToken "YUKI_TOKEN"
  clientEnv <- defaultTelegramClientEnv token
  startBot_ (traceBotDefault botApp) clientEnv

newtype Model = Model{initialized :: Bool}
  deriving Show

data Command
  = Help
  | MtlInfo
  | MtlHolders
  | MtlcityInfo
  | MtlcityHolders
  deriving Show

data Action = NoAction | Action Command
  deriving Show

botApp :: BotApp Model Action
botApp =
  BotApp
    { botInitialModel = Model{initialized = False}
    , botAction       = \u _ -> Action <$> handleUpdate u
    , botHandler      = handleAction
    , botJobs         = []
    }

handleUpdate :: Update -> Maybe Command
handleUpdate =
  parseUpdate
    $   Help <$ (UpdateParser.command "help" <|> UpdateParser.command "start")
    <|> MtlInfo        <$ UpdateParser.command "mtl"
    <|> MtlHolders     <$ UpdateParser.command "mtl_holders"
    <|> MtlcityInfo    <$ UpdateParser.command "mtlcity"
    <|> MtlcityHolders <$ UpdateParser.command "mtlcity_holders"

handleAction :: Action -> Model -> Eff Action Model
handleAction action model@Model{initialized = False} = do
  eff $ NoAction <$ initialize
  handleAction action model{initialized = True}
handleAction NoAction model = do
  pure model
handleAction (Action cmd) model =
  model <# do
    handleCommand cmd
    pure NoAction

initialize :: BotM ()
initialize = pure ()

help :: Text
help =
  "This bot show some stats about MTL fund and its affiliated funds.\n\
  \\n\
  \/mtl – MTL information\n\
  \/mtl_holders – MTL holders\n\
  \/mtlcity – MTLCITY information\n\
  \/mtlcity_holders – MTL holders\n\
  \\n\
  \There may be a delay in a few minutes in data actuality."

handleCommand :: Command -> BotM ()
handleCommand = \case
  Help           -> replyText help
  MtlInfo        -> replyText $ assetExpertUrl mtl
  MtlcityInfo    -> replyText $ assetExpertUrl mtlcity
  MtlHolders     -> replyHolders mtl
  MtlcityHolders -> replyHolders mtlcity

replyHolders :: Fund -> BotM ()
replyHolders fund@Fund{assetName} = do
  knownAccounts <-
    liftIO $ Yaml.decodeFileThrow "../stellar-id/known_accounts.yaml"
  holders <- liftIO $ getHolders fund
  let sumBalance = sum [balance | Holder{balance} <- holders]
  let message =
        Text.unlines
        $   "*" <> assetName <> " holders*"
        :   ""
        :   "```"
        :   "share | tokens | holder"
        :   "------+--------+-------------"
        :   [ Text.intercalate " | "
                [ Text.pack $
                  printf
                    "%4.1f%%"
                    (realToFrac (100 * balance / sumBalance) :: Double)
                , Text.pack $ printf "%6d" (round balance :: Integer)
                , memberName knownAccounts account
                ]
            | Holder{account, balance} <- holders
            ]
        ++  [ "```"
            , "Total supply: " <> tshow (realToFrac sumBalance :: Double)
            ]
  reply (toReplyMessage message){replyMessageParseMode = Just Markdown}

tshow :: Show a => a -> Text
tshow = Text.pack . show

memberName :: Map Text Text -> Text -> Text
memberName knownAccounts account =
  case knownAccounts !? account of
    Just name -> name
    Nothing   -> "..." <> Text.takeEnd 4 account

getCached :: Text -> IO ByteString
getCached url =
  runSqlite cacheFile do
    runMigration migrateAll
    cached <- Persist.get (CacheKey url)
    now <- liftIO getCurrentTime
    case cached of
      Just Cache{cacheResponseBody, cacheUpdated}
        | addUTCTime timeout cacheUpdated > now ->
            pure cacheResponseBody
      _ -> do
        responseBody <-
          liftIO $
          BSL.toStrict . view Wreq.responseBody <$> Wreq.get (Text.unpack url)
        Persist.repsert
          (CacheKey url)
          Cache{cacheResponseBody = responseBody, cacheUpdated = now}
        pure responseBody
  where
    cacheFile = "http_cache.sqlite"
    timeout = secondsToNominalDiffTime 60

getHolders :: Fund -> IO [Holder]
getHolders fund =
  do
    responseBody <- getCached url
    case Aeson.eitherDecodeStrict' responseBody of
      Left err -> fail err
      Right ResponseOk{_embedded = Embedded{records}} ->
        pure
          [ Holder{account, balance = read @Integer balance % 10_000_000}
          | Holder{account, balance} <- records
          ]
  where
    network = "public"
    url =
      mconcat
        [ "https://api.stellar.expert/explorer/", network
        , "/asset/", assetId fund
        , "/holders?limit=100"
        ]

mtl :: Fund
mtl =
  Fund
    { assetName   = "MTL"
    , assetIssuer = "GACKTN5DAZGWXRWB2WLM6OPBDHAMT6SJNGLJZPQMEZBUR4JUGBX2UK7V"
    , treasury = Just "GDX23CPGMQ4LN55VGEDVFZPAJMAUEHSHAMJ2GMCU2ZSHN5QF4TMZYPIS"
    }

mtlcity :: Fund
mtlcity =
  Fund
    { assetName   = "MTLCITY"
    , assetIssuer = "GDUI7JVKWZV4KJVY4EJYBXMGXC2J3ZC67Z6O5QFP4ZMVQM2U5JXK2OK3"
    , treasury    = Nothing
    }

newtype ResponseOk a = ResponseOk{_embedded :: Embedded a}
  deriving anyclass (FromJSON)
  deriving stock (Generic)

newtype Embedded a = Embedded{records :: [a]}
  deriving anyclass (FromJSON)
  deriving stock (Generic)

data Holder' a = Holder{account :: Text, balance :: a}
  deriving (FromJSON, Generic, Show)

type Holder = Holder' Rational

data Fund = Fund{assetName, assetIssuer :: Text, treasury :: Maybe Text}
  deriving Show

assetId :: Fund -> Text
assetId Fund{assetName, assetIssuer} = assetName <> "-" <> assetIssuer

assetExpertUrl :: Fund -> Text
assetExpertUrl fund =
  "https://stellar.expert/explorer/public/asset/" <> assetId fund
