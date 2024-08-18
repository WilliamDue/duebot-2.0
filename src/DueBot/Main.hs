module DueBot.Main (main) where

import Relude
import Data.FileEmbed
import Discord
import DueBot.EventHandler

token :: Text
token = $(embedStringFile ".token")

main :: IO ()
main = do
    userFacingError <- runDiscord $ def
             { discordToken = token
             , discordOnEvent = eventHandler
             , discordOnLog = \s -> putTextLn s >> putTextLn ""
             }

    putTextLn userFacingError
