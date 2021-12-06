module Scanner where

import Syntax
import Data.Void
import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (void)
import Control.Applicative (empty)
import Data.String.Conversions

-- Set the custom error component to Void
-- Set the type of the input stream to Text
-- Text is more efficient than String == [Char]
type Parser = Parsec Void Text

-- A parser that can parses whitespaces
-- space1 skips one or more white space characters.
-- L.space allows patterns for ignoring comments
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineComment blockComment
  where
    lineComment = L.skipLineComment "///" <|> L.skipLineComment "###"
    -- empty means no blockComment allowed
    blockComment = empty

-- Supply spaceConsumer to L.lexmeme to create a parser for tokens
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- Supply spaceConsumer to L.symbol to create a helper parser for symbols
symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

doubleQuotes :: Parser a -> Parser a
doubleQuotes = between (single '"') (single '"')

singleQuotes :: Parser a -> Parser a
singleQuotes = between (single '\'') (single '\'')

semicolon :: Parser ()
semicolon = void $ symbol ";"

comma :: Parser ()
comma = void $ symbol ","

-- try to parse reservedWords, these words should not followed by any other characters (alphaNumChar)
reservedWord :: Text -> Parser ()
reservedWord w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

reservedWords :: [Text]
reservedWords =
  ["bool"
  ,"int"
  ,"void"
  ,"tru"
  ,"fls"
  ,"struct"
  ,"receive"
  ,"print"
  ,"if"
  ,"else"
  ,"while"
  ,"ret"]


strLit :: Parser Text
strLit = do
  -- string is enclosed by double quotes
  -- The predicate use to test tokens is not "
  -- Default to empty string
  content <- doubleQuotes $ takeWhileP Nothing (/= '"')
  -- T.pack convert a String into a Text
  pure $ T.pack ('"' : cs content ++ "\"")

identifier :: Parser Id
identifier = (lexeme . try) (p >>= check)
  where
    -- parse first
    p = fmap T.pack $ (:) <$> letterChar <*> many (alphaNumChar <|> single '_')
    -- then check the validity
    check x = if x `elem` reservedWords
              then fail $ "keyword " <> show x <> " cannot be an identifier"
              else return $ Id x

int :: Parser Int
int = lexeme L.decimal

float :: Parser Double
float = lexeme L.float
