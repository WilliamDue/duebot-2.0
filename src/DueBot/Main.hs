module DueBot.Main (main) where

import Discord
import DueBot.EventHandler
import DueBot.SmallTalk.Compiler
import Relude

main :: IO ()
main = do
  args <- getArgs
  case args of
    [tokenFile, rulesFile] -> runBot tokenFile rulesFile
    _any -> putTextLn "Usage: duebot <token-file> <rules-file>"

runBot :: FilePath -> FilePath -> IO ()
runBot tokenFile rulesFile = do
  token <- decodeUtf8 <$> readFileBS tokenFile
  rules <- createRules rulesFile

  userFacingError <-
    runDiscord
      $ def
        { discordToken = token,
          discordOnEvent = eventHandler rules,
          discordOnLog = \s -> putTextLn s >> putTextLn ""
        }

  putTextLn userFacingError
