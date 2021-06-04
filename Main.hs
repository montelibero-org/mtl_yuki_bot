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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
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
import qualified Data.Text as Text
import           Data.Time (UTCTime, addUTCTime, getCurrentTime,
                            secondsToNominalDiffTime)
import qualified Database.Persist as Persist
import           Database.Persist.Sqlite (runMigration, runSqlite)
import           Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase,
                                      share, sqlSettings)
import           GHC.Generics (Generic)
import qualified Network.Wreq as Wreq
import           Telegram.Bot.API (Update, defaultTelegramClientEnv)
import           Telegram.Bot.Simple (BotApp (..), BotM, Eff, eff, getEnvToken,
                                      replyText, startBot_, (<#))
import           Telegram.Bot.Simple.Debug (traceBotDefault)
import           Telegram.Bot.Simple.UpdateParser as UpdateParser (command,
                                                                   parseUpdate)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
    Cache
      Id            String
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

data Command = Help | MtlInfo | MtlHolders
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
    <|> MtlInfo    <$ UpdateParser.command "mtl"
    <|> MtlHolders <$ UpdateParser.command "mtl_holders"

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

handleCommand :: Command -> BotM ()
handleCommand = \case
  Help ->
    replyText
      "This bot show some stats about MTL fund and its affiliated funds.\n\
      \\n\
      \/mtl – MTL information\n\
      \/mtl_holders – MTL holders\n\
      \\n\
      \There may be a delay in a few minutes in data actuality."
  MtlInfo ->
    replyText
      "https://stellar.expert/explorer/public/asset/\
      \MTL-GACKTN5DAZGWXRWB2WLM6OPBDHAMT6SJNGLJZPQMEZBUR4JUGBX2UK7V"
  MtlHolders -> do
    holders <- liftIO $ getHolders mtl
    replyText $ Text.pack $ show holders

getCached :: String -> IO ByteString
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
          liftIO $ BSL.toStrict . view Wreq.responseBody <$> Wreq.get url
        Persist.insertKey
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
      Left err                                        -> fail err
      Right ResponseOk{_embedded = Embedded{records}} -> pure records
  where
    network = "public"
    url =
      concat
        [ "https://api.stellar.expert/explorer/", network
        , "/asset/", assetId fund
        , "/holders"
        ]

mtl :: Fund
mtl =
  Fund
    { assetName   = "MTL"
    , assetIssuer = "GACKTN5DAZGWXRWB2WLM6OPBDHAMT6SJNGLJZPQMEZBUR4JUGBX2UK7V"
    , treasury = Just "GDX23CPGMQ4LN55VGEDVFZPAJMAUEHSHAMJ2GMCU2ZSHN5QF4TMZYPIS"
    }

-- mtlcity :: Fund
-- mtlcity =
--   Fund
--     { assetName   = "MTLCITY"
--     , assetIssuer = "GDUI7JVKWZV4KJVY4EJYBXMGXC2J3ZC67Z6O5QFP4ZMVQM2U5JXK2OK3"
--     , treasury    = Nothing
--     }

newtype ResponseOk a = ResponseOk{_embedded :: Embedded a}
  deriving anyclass (FromJSON)
  deriving stock (Generic)

newtype Embedded a = Embedded{records :: [a]}
  deriving anyclass (FromJSON)
  deriving stock (Generic)

data Holder = Holder{account, balance :: String}
  deriving (FromJSON, Generic, Show)

data Fund = Fund{assetName, assetIssuer :: String, treasury :: Maybe String}

assetId :: Fund -> String
assetId Fund{assetName, assetIssuer} = assetName <> "-" <> assetIssuer
