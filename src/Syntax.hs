module Syntax where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Char

-- Binary Operator
data Op = Plus
        | Minus
        | Times
        | Divide
        | And
        | Or
        | Equals
        | NotEquals
        | Less
        | Greater
        | LessEq
        | GreaterEq
        deriving (Show, Eq)

-- Unary Operator
data UniOp = Not
           | Neg
           deriving (Show, Eq)

-- Identifier
data Id = Id Text
        deriving (Show, Eq, Ord)

-- Expression
data Expr = BinOp Op Expr Expr
          | UniOp UniOp Expr
          | Assign Expr Expr
          | IntLit Int
          | StrLit Text
          | FloatLit Double
          | BoolLit Bool
          | Call Id [Expr]
          | Dot Expr Expr
          | Var Id
          | NoExpr
          deriving (Show, Eq)

-- Statement
data Stmt = If Expr Stmt Stmt
          | While Expr Stmt
          | Block [Stmt]
          | Expr Expr
          | Return Expr
          | PlusPlus Expr
          | MinusMinus Expr
          deriving (Show, Eq)

-- Type
data Type = TypeInt
          | TypeBool
          | TypeFloat
          | TypeVoid
          | TypeStr
          | TypeStruct Id
          deriving (Show, Eq)

-- Variable Binding
data Bind = Bind { bindType :: Type, bindName :: Id }
          deriving (Show, Eq)

-- Struct
data Struct = Struct { structName :: Id, structFields :: [Bind] }
            deriving (Show, Eq)

-- Function
data Function  = Function { retType :: Type
                          , name :: Id
                          , formals :: [Bind]
                          , locals :: [Bind]
                          , body :: [Stmt]}
               deriving (Show, Eq)

-- Top-level, the program
data Program = Program [Struct] [Bind] [Function]
             deriving (Show, Eq)

-- The following code are used to pretty print the AST
-- x <+> y = x <> space <> y
instance Pretty Op where
  pretty = \case
    Plus -> "+"
    Minus -> "-"
    Times -> "*"
    Divide -> "/"
    And -> "&&"
    Or -> "||"
    Equals -> "=="
    NotEquals -> "!="
    Less -> "<"
    Greater -> ">"
    LessEq -> "<="
    GreaterEq -> ">="

instance Pretty UniOp where
  pretty = \case
    Not -> "!"
    Neg -> "-"

instance Pretty Struct where
  pretty Struct {structName = name, structFields = fields} = "struct" <+>
      -- lbrace: {
      -- hardline: a line break
      -- vsep: puts a line break between all entries in its argument
      pretty name <+> lbrace <> hardline <> indent 4 (vsep (map (\field -> pretty field <> semi) fields)) <> hardline <> rbrace <> semi

instance Pretty Type where
  pretty = \case
    TypeInt -> "int"
    TypeBool -> "bool"
    TypeVoid -> "void"
    TypeFloat -> "float"
    TypeStr -> "string"
    TypeStruct id -> "struct" <+> pretty id

instance Pretty Bind where
  pretty Bind {bindType = typ, bindName = name} = pretty typ <+> pretty name

instance Pretty Expr where
  pretty = \case
    -- hsep: puts a space between all entries in its argument
    BinOp op lhs rhs -> hsep [pretty lhs, pretty op, pretty rhs]
    UniOp op e -> pretty op <> parens (pretty e)
    Assign lhs rhs -> pretty lhs <+> equals <+> pretty rhs
    IntLit i -> pretty i
    -- dquotes: place double quotes around its argument
    StrLit s -> dquotes $ pretty s
    FloatLit f -> pretty f
    BoolLit b -> if b then "true" else "false"
    -- tupled: use parentheses to enclose its argument
    Call f es -> pretty f <> tupled (map pretty es)
    Dot struct field -> pretty struct <> dot <> pretty field
    Var i -> pretty i
    NoExpr -> mempty

instance Pretty Id where
  pretty (Id name) = pretty name

instance Pretty Stmt where
  pretty = \case
    If predicate thenClause elseClause -> "if" <+> parens (pretty predicate) <+> pretty thenClause <> prettyElseClause
      where
        prettyElseClause =
          case elseClause of
            -- Empty if the else block is empty
            Block [] -> mempty
            _ -> hardline <> "else" <+> pretty elseClause
    While predicate body -> "while" <+> parens (pretty predicate) <+> pretty body
    Block ss -> lbrace <> hardline <> indent 4 (vsep (map pretty ss)) <> hardline <> rbrace
    Expr e -> pretty e <> semi
    Return e -> "return" <+> pretty e <> semi
    PlusPlus e -> "++" <+> pretty e <> semi
    MinusMinus e -> "--" <+> pretty e <> semi

instance Pretty Function where
  pretty Function { retType = retType, name = name, formals = formals, locals = locals, body = body} =
    pretty retType <+> pretty name <> tupled (map pretty formals)
    <> hardline <> lbrace <> hardline <>
    indent 4 (vsep (map decl locals ++ map pretty body))
    <> hardline <> rbrace <> hardline

instance Pretty Program where
  pretty (Program structs binds funcs) = vsep
    (map pretty structs ++ map decl binds ++ map pretty funcs)

decl :: Pretty a => a -> Doc ann
decl bind = pretty bind <> semi
