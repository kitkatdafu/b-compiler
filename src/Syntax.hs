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
        deriving (Show, Eq)

data Expr = Term
          | BinOp Op Expr Expr
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
          | TypeVoid
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
