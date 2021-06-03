{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

import           Control.Applicative ((<|>))
import           Control.Monad (void)
import           Data.Aeson (ToJSON)
import           Data.Proxy (Proxy (..))
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Servant.API (JSON, Post, ReqBody, (:>))
import           Servant.Client (ClientM, client)
import           Telegram.Bot.API (Response, Update, defaultTelegramClientEnv)
import           Telegram.Bot.Simple (BotApp (..), BotM, Eff, eff, getEnvToken,
                                      liftClientM, replyText, startBot_, (<#))
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

data Command = Start | Help
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
    $   Help  <$ UpdateParser.command "help"
    <|> Start <$ UpdateParser.command "start"

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
initialize =
  void $
  liftClientM $
  setMyCommands
    [ BotCommand{command = "help",  description = "Show help"}
    , BotCommand{command = "start", description = "Start bot"}
    ]

handleCommand :: Command -> BotM ()
handleCommand = \case
  Help  -> replyText "...help..."
  Start -> replyText "...start..."


-- ** 'BotCommand'

-- | This object represents a bot command.
data BotCommand = BotCommand
  { command :: Text
    -- ^ Text of the command, 1-32 characters.
    -- Can contain only lowercase English letters, digits and underscores.
  , description :: Text
    -- ^ Description of the command, 3-256 characters.
  }
  deriving (Generic, ToJSON)


-- ** 'setMyCommands'

type SetMyCommands
  = "setMyCommands"
  :> ReqBody '[JSON] [BotCommand]
  :> Post '[JSON] (Response Bool)

-- | Use this method to change the list of the bot's commands.
-- Returns @True@ on success.
setMyCommands :: [BotCommand] -> ClientM (Response Bool)
setMyCommands = client (Proxy @SetMyCommands)
