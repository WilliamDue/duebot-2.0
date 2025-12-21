module DueBot.EventHandler (eventHandler) where

import Discord
import Discord.Requests qualified as R
import Discord.Types
import DueBot.SmallTalk.Compiler
import Relude hiding (lift)
import System.Random

errorMessage :: Text
errorMessage = "Error: Message <@150985356827820032>, their code is terrible."

eventHandler :: Rules -> Event -> DiscordHandler ()
eventHandler rules event = case event of
  MessageCreate m -> unless (fromBot m) $ do
    let content = messageContent m
        response = ruleMatch rules content

    case response of
      Just r ->
        if null r
          then pure ()
          else do
            idx <- randomRIO (0, length r - 1)
            let result = fromMaybe errorMessage $ r !!? idx
            void $ restCall (R.CreateMessage (messageChannelId m) result)
      Nothing -> pure ()
  _anyOtherFailure -> pure ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor
