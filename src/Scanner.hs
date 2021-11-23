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

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineComment blockComment
  where
    lineComment = L.skipLineComment "///" <|> L.skipLineComment "###"
    blockComment = empty
    
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

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
  content <- doubleQuotes $ takeWhileP Nothing (/= '"')
  pure $ T.pack (read ('"' : cs content ++ "\""))

charLit :: Parser Int
charLit =
  singleQuotes $ (ord <$> satisfy (`notElem` ['\\', '\''])) <|> (single '\\' >> int)

identifier :: Parser Id
identifier = (lexeme . try) (p >>= check)
  where
    p = fmap T.pack $ (:) <$> letterChar <*> many (alphaNumChar <|> single '_')
    check x = if x `elem` reservedWords
              then fail $ "keyword " <> show x <> " cannot be an identifier"
              else return $ Id x

int :: Parser Int
int = lexeme L.decimal

float :: Parser Double
float = lexeme L.float


