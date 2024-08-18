module DueBot.SmallTalk.Parser (
  rulesFromText,
  Rule (..),
  Atom (..),
  Word (..),
  Result (..)) where

import Relude hiding (many, Word)
import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char (char, space1)
import Text.Megaparsec.Char.Lexer qualified as Lexer

data Atom = Literal !Text | Pattern !Text deriving (Show, Eq, Ord)
newtype Word = Word [Atom] deriving (Show, Eq, Ord)
newtype Result = Result Text deriving (Show, Eq, Ord)
data Rule = Rule ![Word] ![Result] deriving (Show, Eq, Ord)

type Parser = Parsec Void Text

space :: Parser ()
space = Lexer.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

leftEscapeChars :: [Char]
leftEscapeChars = ['\\', '"', '/']

pEscapeChars :: [Char] -> Parser Char
pEscapeChars ls = char '\\' *> choice (map char ls)

pValid :: [Char] -> Parser Char
pValid ls = satisfy (\c -> c `notElem` ls && isPrint c)

pLeftChar :: Parser Char
pLeftChar =
  choice
    [ pEscapeChars leftEscapeChars,
      pValid leftEscapeChars
    ]

many1 :: Parser a -> Parser [a]
many1 p = liftA2 (:) p (many p)

pLiteral :: Parser Atom
pLiteral = Literal . toText <$> many1 pLeftChar

pPattern :: Parser Atom
pPattern = Pattern . toText <$> (char '/' *> aux <* char '/')
  where
    aux = many1 pLeftChar

pWord :: Parser Word
pWord = Word <$> lexeme (char '"' *> auxiliary <* char '"')
  where
    auxiliary = many1 $ choice [pPattern, pLiteral]

pWords :: Parser [Word]
pWords = many1 pWord

rightEscapeChars :: [Char]
rightEscapeChars =
  ['\\', '"']

pRightChar :: Parser Char
pRightChar =
  choice
    [ pEscapeChars rightEscapeChars,
      pValid rightEscapeChars
    ]

pResult :: Parser Result
pResult = Result . toText <$> lexeme (char '"' *> aux <* char '"')
  where
    aux = many1 pRightChar

pResults :: Parser [Result]
pResults = many1 pResult

pRule :: Parser Rule
pRule = lexeme $ do
  _words <- pWords
  _ <- lexeme $ char ':'
  results <- pResults
  _ <- lexeme $ char ';'
  return $ Rule _words results

pRules :: Parser [Rule]
pRules = many pRule

rulesFromText :: FilePath -> Text -> Either Text [Rule]
rulesFromText fname s =
  either (Left . toText . errorBundlePretty) Right
  $ parse (pRules <* eof) fname s
