module Main where

import Data.Text
import qualified Data.Text.IO as TIO
import Data.FileEmbed
import Discord
import DueBot.

token :: Text
token = $(embedStringFile ".token")

-- | Replies "pong" to every message that starts with "ping"
main :: IO ()
main = do
    userFacingError <- runDiscord $ def
             { discordToken = token
             , discordOnEvent = eventHandler
             , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
             } -- if you see OnLog error, post in the discord / open an issue

    TIO.putStrLn userFacingError
