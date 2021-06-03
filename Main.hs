{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative ((<|>))
import           Control.Lens ((^.))
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (FromJSON)
import qualified Data.Text as Text
import           GHC.Generics (Generic)
import           Network.Wreq (asJSON, get, responseBody)
import           Telegram.Bot.API (Update, defaultTelegramClientEnv)
import           Telegram.Bot.Simple (BotApp (..), BotM, Eff, eff, getEnvToken,
                                      replyText, startBot_, (<#))
import           Telegram.Bot.Simple.Debug (traceBotDefault)
import           Telegram.Bot.Simple.UpdateParser as UpdateParser (command,
                                                                   parseUpdate)

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
      \"
  MtlInfo ->
    replyText
      "https://stellar.expert/explorer/public/asset/\
      \MTL-GACKTN5DAZGWXRWB2WLM6OPBDHAMT6SJNGLJZPQMEZBUR4JUGBX2UK7V"
  MtlHolders -> do
    holders <- liftIO $ getHolders mtl
    replyText $ Text.pack $ show holders

getHolders :: Fund -> IO [Holder]
getHolders fund =
  do
    r <- asJSON =<< get url
    let ResponseOk{_embedded = Embedded{records}} = r ^. responseBody
    pure records
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
  deriving (FromJSON, Generic)

newtype Embedded a = Embedded{records :: [a]}
  deriving (FromJSON, Generic)

data Holder = Holder{account, balance :: String}
  deriving (FromJSON, Generic, Show)

data Fund = Fund{assetName, assetIssuer :: String, treasury :: Maybe String}

assetId :: Fund -> String
assetId Fund{assetName, assetIssuer} = assetName <> "-" <> assetIssuer
