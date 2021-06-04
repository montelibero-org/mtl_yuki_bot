{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative ((<|>))
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import           Data.Map.Strict (Map, (!?))
import           Data.Maybe (catMaybes)
import           Data.Ratio ((%))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml
import           GHC.Generics (Generic)
import           Telegram.Bot.API (ParseMode (Markdown), Update,
                                   defaultTelegramClientEnv)
import           Telegram.Bot.Simple (BotApp (..), BotM, Eff, getEnvToken,
                                      reply, replyMessageParseMode, replyText,
                                      startBot_, toReplyMessage, (<#))
import           Telegram.Bot.Simple.Debug (traceBotDefault)
import           Telegram.Bot.Simple.UpdateParser (parseUpdate)
import qualified Telegram.Bot.Simple.UpdateParser as UpdateParser
import           Text.Printf (printf)

import           HTTP (getCached)

main :: IO ()
main = do
  token     <- getEnvToken "YUKI_TOKEN"
  clientEnv <- defaultTelegramClientEnv token
  startBot_ (traceBotDefault botApp) clientEnv

data Model = NoModel
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
    { botInitialModel = NoModel
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
handleAction NoAction     model = pure model
handleAction (Action cmd) model =
  model <# do
    handleCommand cmd
    pure NoAction

help :: Text
help =
  "This bot show some stats about MTL fund and its affiliated funds.\n\
  \\n\
  \/mtl – MTL information\n\
  \/mtl_holders – MTL holders\n\
  \/mtlcity – MTLCITY information\n\
  \/mtlcity_holders – MTL holders\n\
  \\n\
  \There may be a delay in a few minutes in data freshness."

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

data KnownAccount = KnownAccount
  { name     :: Maybe Text
  , telegram :: Maybe Text
  }
  deriving (FromJSON, Generic)

memberName :: Map Text KnownAccount -> Text -> Text
memberName knownAccounts account =
  case knownAccounts !? account of
    Just KnownAccount{name = Nothing, telegram = Nothing} -> def
    Just KnownAccount{name, telegram} ->
      Text.unwords $ catMaybes [name, telegram]
    Nothing -> def
  where
    def = "..." <> Text.takeEnd 4 account

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
