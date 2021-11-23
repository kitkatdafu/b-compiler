module Parser (programP, runParser, errorBundlePretty) where 

import Syntax
import Scanner
import Text.Megaparsec
import Control.Monad.Combinators.Expr
import Control.Applicative (liftA2, liftA3)
import Data.Either

opTable :: [[Operator Parser Expr]]
opTable =
  [[InfixL $ Dot <$ symbol "."]
  ,[unary (UniOp Neg) "-"
   ,unary (UniOp Not) "!"]
  ,[infixL Times "*"
   ,infixL Divide "/"]
  ,[infixL Plus "+"
   ,infixL Minus "-"]
  ,[infixL Equals "=="
   ,infixL NotEquals "!="
   ,infixL Less "<"
   ,infixL LessEq "<="
   ,infixL Greater ">"
   ,infixL GreaterEq ">="]
  ,[infixL And "&&"
   ,infixL Or "||"]
  ,[InfixR $ Assign <$ symbol "="]]
  where
    unary op sym = Prefix $ foldr1 (.) <$> some (op <$ symbol sym)
    infixL op sym = InfixL $ BinOp op <$ symbol sym

termP :: Parser Expr
termP = parens exprP
    <|> try (FloatLit <$> float)
    <|> IntLit <$> int
    <|> BoolLit <$> (True <$ reservedWord "true" <|> False <$ reservedWord "false")
    <|> try (Call <$> identifier <*> parens (exprP `sepBy` comma))
    <|> StrLit <$> strLit
    <|> Var <$> identifier
  
exprP :: Parser Expr
exprP = makeExprParser termP opTable

exprMaybe :: Parser Expr
exprMaybe = option NoExpr exprP

structP :: Parser Struct
structP = Struct <$> (reservedWord "struct" *> identifier) <*> braces (many vdeclP)

typeP :: Parser Type
typeP = TypeInt <$ reservedWord "int"
     <|> TypeBool  <$ reservedWord "bool"
     <|> TypeVoid  <$ reservedWord "void"
     <|> TypeStruct <$> (reservedWord "struct" *> identifier)

vdeclP :: Parser Bind
vdeclP = Bind <$> typeP <*> identifier <* semicolon

statementP :: Parser Stmt
statementP = Expr <$> exprP <* semicolon
  <|> Return <$> (reservedWord "return" *> exprMaybe <* semicolon)
  <|> Block <$> braces (many statementP)
  <|> ifP
  <|> forP
  <|> whileP
  <|> plusplusP
  <|> minusminusP

plusplusP :: Parser Stmt
plusplusP = PlusPlus <$> (symbol "++" *> exprP <* semicolon)
minusminusP :: Parser Stmt
minusminusP = MinusMinus <$> (symbol "--" *> exprP <* semicolon)

ifP :: Parser Stmt
ifP = liftA3 If (reservedWord "if" *> parens exprP) statementP maybeElse
  where maybeElse = option (Block []) (reservedWord "else" *> statementP)

forP :: Parser Stmt
forP = do
  reservedWord "for"
  (e1, e2, e3) <- parens $ liftA3 (,,) (exprMaybe <* semicolon) (exprP <* semicolon) exprMaybe
  For e1 e2 e3 <$> statementP

whileP :: Parser Stmt
whileP = liftA2 While (reservedWord "while" *> parens exprP) statementP

fdeclP :: Parser Function
fdeclP = Function <$> typeP <*> identifier <*> formalsP
  <*> (symbol "{" *> many vdeclP)
  <*> (many statementP <* symbol "}")

formalsP :: Parser [Bind]
formalsP = parens $ formalP `sepBy` comma
  where formalP = liftA2 Bind typeP identifier

programP :: Parser Program
programP = between spaceConsumer eof $ do
  structsOrGlobals <- many $ try (Left <$> structP) <|> (Right <$> try vdeclP)
  let structs = lefts structsOrGlobals
      globals = rights structsOrGlobals
  Program structs globals <$> many fdeclP
