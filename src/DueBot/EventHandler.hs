module DueBot.EventHandler (eventHandler) where

import Relude
import Discord
import Discord.Types
import qualified Discord.Requests as R
import Data.FileEmbed
import DueBot.SmallTalk.Parser
import DueBot.SmallTalk.Compiler
import Text.RE.TDFA.Text
import Data.IntMap.Lazy (lookup)

rules :: Either Text [Rule]
rules = rulesFromText "rules.cfg" $ $(embedStringFile "rules.cfg")

regex :: RE
regex = case rules >>= compileRules of
          Left x -> error x
          Right x -> x

table :: IntMap [Text]
table = case createTableRules <$> rules of
          Left x -> error x
          Right x -> x

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
    MessageCreate m -> unless (fromBot m) $ do
        let content = messageContent m
        let response = ruleMatch regex content >>= (`lookup` table)
        case response of
          Just (r:_) -> void $ restCall (R.CreateMessage (messageChannelId m) r)
          _x -> pure ()
    _anyOtherFailure -> return ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor
