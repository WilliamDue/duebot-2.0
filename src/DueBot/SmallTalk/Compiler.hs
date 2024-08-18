module DueBot.SmallTalk.Compiler (
  compileRules,
  createTableRules,
  ruleMatch) where

import DueBot.SmallTalk.Parser
import Relude hiding (Word, intercalate)
import Data.HashMap.Lazy (keys)
import Data.Text (intercalate)
import Text.RE.Replace
import Text.RE.TDFA.Text

escapeREText :: Text -> Text
escapeREText = toText . escapeREString . toString

wordToRegex :: Word -> Text
wordToRegex (Word _ws) = foldMap toRegex _ws
  where
    toRegex (Literal l) = escapeREText l
    toRegex (Pattern p) = p

wordsToRegex :: Int -> [Word] -> Text
wordsToRegex i =
  (("${"<>show i<>"}(")<>)
  . (<>")")
  . intercalate "|"
  . fmap wordToRegex

firstCapture :: Match Text -> Maybe Int
firstCapture =
  (>>= (readMaybe . toString))
  . fmap getCaptureName
  . listToMaybe
  . keys
  . captureNames

compileRules :: [Rule] -> Either Text RE
compileRules rules =
  compileRegex
  $ toString
  $ intercalate "|"
  $ zipWith wordsToRegex [0..] (map words' rules)
  where
    words' (Rule w _) = w

createTableRules :: [Rule] -> IntMap [Text]
createTableRules rules =
  fromList $ zip [(0 :: Int)..] (map results rules)
  where
    unwrap (Result r) = r
    results (Rule _ r) = map unwrap r

ruleMatch :: RE -> Text -> Maybe Int
ruleMatch _re text = 
  if matched match
    then firstCapture match 
    else Nothing
  where
    match = text ?=~ _re

-- main :: IO ()
-- main = do
--  _re <- compileRegex "-${0}([a-z]+)-"
--  let match = "haystack contains the -word- you want -test-" ?=~ _re
--  if matched match
--    then print $ firstCapture match 
--    else putTextLn "(no match)"
