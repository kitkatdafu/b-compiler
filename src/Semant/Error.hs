module Semant.Error where

import Syntax
import Data.Text (Text)
import Data.Text.Prettyprint.Doc

data BindingLoc = F Function | S Struct | TopLevel
                deriving Show
data BindingKind = Duplicate | Void
                 deriving Show
data SymbolKind = Variable | Func
                deriving Show
data VarKind = Global | Formal | Local | StructField
             deriving (Show, Eq, Ord)

data SemantError = IllegalBinding Id BindingKind VarKind BindingLoc
                 | UndefinedSymbol Id SymbolKind Expr
                 | TypeError { expected :: [Type], got :: Type, errorLoc :: Stmt }
                 | ArgError { nExpected :: Int, nGot :: Int, callSite :: Expr}
                 | Redeclaration Id
                 | AssignmentError { lhs :: Expr, rhs :: Expr }
                 | AccessError { struct :: Expr, field :: Expr }
                 deriving (Show)

instance Pretty VarKind where
  pretty = unsafeViaShow

instance Pretty SymbolKind where
  pretty = \case
    Variable -> "variable"
    Func -> "function"

instance Pretty BindingKind where
  pretty = unsafeViaShow

instance Pretty SemantError where
  pretty = \case
    IllegalBinding nm bindKind varKind loc ->
      "Error: Illegal" <+> pretty bindKind <+> pretty varKind <+>
      "binding," <+> pretty nm <+> case loc of
      F f -> "in function" <+> pretty (name f)
      S (Struct sname _) -> "in struct" <+> pretty sname
      TopLevel -> mempty

    UndefinedSymbol nm symKind expr ->
      "Undefined" <+> pretty symKind <+> pretty nm <+>
      "referenced in:" <> hardline <> pretty expr

    TypeError expected got stmt ->
      "Type error: expected one of" <+> pretty expected <+> "but got"
      <+> pretty got <> ". Error occured in statement:" <> hardline <> pretty stmt
    ArgError nExpected nGot callSite ->
      "Argument error: function expected" <+> pretty nExpected <+>
      "arguments, but was called with" <+> pretty nGot <+> "arguments"
      <> ". Error occured in call:" <> hardline <> pretty callSite

    Redeclaration name -> "Error: redeclaration of function" <+> pretty name

    AssignmentError lhs rhs ->
      "Cannot assign" <+> pretty rhs <+> "to" <+> pretty lhs

    AccessError struct field ->
      "Cannot access" <+> pretty struct <+> "with" <+> pretty field
