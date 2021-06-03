{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative ((<|>))
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

data Command = Help
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
  parseUpdate $
  Help <$ (UpdateParser.command "help" <|> UpdateParser.command "start")

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
    replyText "This bot show some stats about MTL fund and its affiliated funds"
