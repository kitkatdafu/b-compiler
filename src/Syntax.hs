module Syntax where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Char

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

data UniOp = Not
           | Neg
           deriving (Show, Eq)

data Id = Id Text
        deriving (Show, Eq, Ord)

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


data Stmt = If Expr Stmt Stmt
          | For Expr Expr Expr Stmt
          | While Expr Stmt
          | Block [Stmt]
          | Expr Expr
          | Return Expr
          | PlusPlus Expr
          | MinusMinus Expr
          deriving (Show, Eq)

data Type = TypeInt
          | TypeBool
          | TypeFloat
          | TypeVoid
          | TypeStr
          | TypeStruct Id
          deriving (Show, Eq)

data Bind = Bind { bindType :: Type, bindName :: Id }
          deriving (Show, Eq)

data Struct = Struct { structName :: Id, structFields :: [Bind] }
            deriving (Show, Eq)

data Function  = Function { retType :: Type
                          , name :: Id
                          , formals :: [Bind]
                          , locals :: [Bind]
                          , body :: [Stmt]}
               deriving (Show, Eq)

data Program = Program [Struct] [Bind] [Function]
             deriving (Show, Eq)

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
  pretty (Struct nm binds) = "struct" <+>
      pretty nm <+> lbrace <> hardline <> indent 4 (vsep (map (\b -> pretty b <> ";") binds))
      <> hardline <> rbrace <> ";"

instance Pretty Type where
  pretty = \case
    TypeInt -> "int"
    TypeBool -> "bool"
    TypeVoid -> "void"
    TypeStruct n -> "struct" <+> pretty n

instance Pretty Bind where
  pretty (Bind ty nm) = pretty ty <+> pretty nm

instance Pretty Expr where
  pretty = \case
    BinOp op lhs rhs -> hsep [pretty lhs, pretty op, pretty rhs]
    UniOp op e -> pretty op <> parens (pretty e)
    Assign lhs rhs -> pretty lhs <+> "=" <+> pretty rhs
    IntLit i -> pretty i
    StrLit s -> dquotes $ pretty s
    FloatLit f -> pretty f
    BoolLit b -> if b then "true" else "false"
    Call f es -> pretty f <> tupled (map pretty es)
    Dot struct field -> pretty struct <> "." <> pretty field
    Var i -> pretty i
    NoExpr -> mempty

instance Pretty Id where
  pretty = \case
    Id t -> pretty t

instance Pretty Stmt where
  pretty = \case
    If pred cons alt ->
      "if" <+> parens (pretty pred) <+> pretty cons <> prettyAlt
      where
        prettyAlt =
          case alt of
            Block [] -> mempty
            _ -> hardline <> "else" <+> pretty alt
    For init cond inc body -> "for" <+>
      encloseSep lparen rparen semi [pretty init, pretty cond, pretty inc]
      <+> pretty body
    While cond body -> "while" <+> parens (pretty cond) <+> pretty body
    Block ss -> lbrace <> hardline <> indent 4 (vsep (map pretty ss))
      <> hardline <> rbrace
    Expr e -> pretty e <> semi
    Return e -> "return" <+> pretty e <> semi
    PlusPlus e -> "++" <+> pretty e <> semi
    MinusMinus e -> "--" <+> pretty e <> semi

instance Pretty Function where
  pretty (Function typ name formals locals body) =
    pretty typ <+> pretty name <> tupled (map pretty formals)
    <> hardline <> lbrace <> hardline <>
    indent 4 (hardsep (map decl locals ++ map pretty body))
    <> hardline <> rbrace <> hardline

instance Pretty Program where
  pretty (Program structs binds funcs) = hardsep
    (map pretty structs ++ map decl binds ++ map pretty funcs)

decl :: Pretty a => a -> Doc ann
decl bind = pretty bind <> semi

-- | Separates many docs with hardlines
hardsep :: [Doc ann] -> Doc ann
hardsep = concatWith (\x y -> x <> hardline <> y)
