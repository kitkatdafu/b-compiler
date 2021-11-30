module Sast where

import Syntax
import Data.Text (Text)

type SExpr = (Type, SExpr')

data SExpr' = SBinOp Op SExpr SExpr
            | SUniOp UniOp SExpr
            | SAssign LValue SExpr
            | SIntLit Int
            | SStrLit Text
            | SFloatLit Double
            | SBoolLit Bool
            | LVal LValue
            | SCall Id [SExpr]
            | SNoExpr
            deriving (Show, Eq)

data LValue = SDot LValue Int
            | SVar Id
            deriving (Show, Eq)

data SStmt = SIf SExpr SStmt SStmt
           | SDoWhile SExpr SStmt
           | SBlock [SStmt]
           | SExpr SExpr
           | SReturn SExpr
           | SPlusPlus SExpr
           | SMinusMinus SExpr
           deriving (Show, Eq)

data SFunction = SFunction { sretType :: Type
                           , sname :: Id
                           , sformals :: [Bind]
                           , slocals :: [Bind]
                           , sbody :: SStmt
                           }
                 deriving (Show, Eq)

type SProgram = ([Struct], [Bind], [SFunction])
  
