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

firstCapture :: Match Text -> Maybe Int
firstCapture match =
  (>>= readMaybe . toString) $ getCaptureName <$> maybe_capture
  where
    capture_names = keys $ captureNames match
    isCapture = isJust . flip captureMaybe match . IsCaptureName
    maybe_capture = find isCapture capture_names

ruleMatch :: RE -> Text -> Maybe Int
ruleMatch _re text =
  if matched match
    then firstCapture match 
    else Nothing
  where
    match = text ?=~ _re
