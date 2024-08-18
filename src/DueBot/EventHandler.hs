module DueBot.EventHandler (eventHandler) where

import Relude hiding (lift)
import Discord
import Discord.Types
import qualified Discord.Requests as R
import Data.FileEmbed
import DueBot.SmallTalk.Parser
import DueBot.SmallTalk.Compiler
import Text.RE.TDFA.Text
import Data.IntMap.Lazy (lookup)
import System.Random

rulesText :: Text
rulesText = $(embedStringFile "rules.cfg")

rules :: Either Text [Rule]
rules = rulesFromText "rules.cfg" rulesText 

regex :: Either Text RE
regex = rules >>= compileRules

table :: Either Text (IntMap [Text])
table = createTableRules <$> rules

errorMessage :: Text
errorMessage = "Error: message @due their code is terrible."

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
    MessageCreate m -> unless (fromBot m) $ do
        let content = messageContent m
        let response = do
              r <- regex
              t <- table
              return $ ruleMatch r content >>= (`lookup` t)
              
        case response of
          Left e -> void $ restCall (R.CreateMessage (messageChannelId m) e)
          Right (Just r) ->
            if null r
              then pure ()
              else do
                idx <- randomRIO (0, length r - 1)
                let result = fromMaybe errorMessage $ r !!? idx
                void $ restCall (R.CreateMessage (messageChannelId m) result)
          Right Nothing -> pure ()
    _anyOtherFailure -> pure ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor
