{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}

module Semant (checkProgram) where

import Syntax
import Sast
import Utils
import Semant.Error
import Semant.Analysis

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.List (find, findIndex)

type Vars = M.Map (Id, VarKind) Type
type Funcs = M.Map Id Function
type Structs = [Struct]
data Env = Env { vars :: Vars
               , funcs :: Funcs
               , structs :: Structs
               }

type Semant = ExceptT SemantError (State Env)

checkBinds :: VarKind -> BindingLoc -> [Bind] -> Semant [Bind]
checkBinds kind loc binds = do
  forM binds $ \case
    Bind TypeVoid name -> throwError $ IllegalBinding name Void kind loc
    Bind ty name -> do
      vars <- gets vars
      when (M.member (name, kind) vars) $ throwError (IllegalBinding name Duplicate kind loc)
      modify $ \env -> env {vars = M.insert (name, kind) ty vars}
      pure $ Bind ty name
 
checkFields :: Struct -> Semant Struct
checkFields s@(Struct name fields) = do
  fields' <- foldM addField M.empty fields
  pure $ Struct name (M.elems fields')
  where
    addField acc field@(Bind t name) = case t of
        TypeVoid -> throwError $ IllegalBinding name Void StructField (S s)
        _ -> if M.member name acc
             then throwError $ IllegalBinding name Duplicate StructField (S s)
             else pure $ M.insert name field acc


checkExpr :: Expr -> Semant SExpr
checkExpr expr = case expr of
  IntLit i -> pure (TypeInt, SIntLit i)
  StrLit s -> pure (TypeStr, SStrLit s)
  BoolLit b -> pure (TypeBool, SBoolLit b)
  NoExpr -> pure (TypeVoid, SNoExpr)  

  Var id -> do
    vars <- gets vars
    let foundVars = map (\kind -> M.lookup (id, kind) vars) [Local, Formal, Global]
    case join $ find isJust foundVars of
      Nothing -> throwError $ UndefinedSymbol id Variable expr
      Just ty -> pure (ty, LVal $ SVar id)

  BinOp op lhs rhs -> do
    lhs'@(t1, _) <- checkExpr lhs
    rhs'@(t2, _) <- checkExpr rhs
    let
      assertSym = unless (t1 == t2) $ throwError $ TypeError [TypeInt] t1 (Expr expr)
      checkArith = do
        unless (isNumeric t1) $ throwError $ TypeError [TypeInt] t1 (Expr expr)
        pure (t1, SBinOp op lhs' rhs')
      checkBool = do
        unless (t1 == TypeBool) $ throwError $ TypeError [TypeBool] t1 (Expr expr)
        pure (t1, SBinOp op lhs' rhs')
    case op of
      Plus -> assertSym >> checkArith
      Minus -> assertSym >> checkArith
      Times -> assertSym >> checkArith
      Divide -> assertSym >> checkArith
      And -> assertSym >> checkBool
      Or -> assertSym >> checkBool
      relational -> do
        assertSym
        unless (isNumeric t1) $ throwError $ TypeError [TypeInt] t1 (Expr expr)
        pure (TypeBool, SBinOp op lhs' rhs')
  UniOp op e -> do
    e'@(ty, _) <- checkExpr e
    case op of
      Neg -> do
        unless (isNumeric ty)
          $ throwError (TypeError [TypeInt] ty (Expr expr))
        pure (ty, SUniOp Neg e')
      Not -> do
        unless (ty == TypeBool) $ throwError $
          TypeError [TypeBool] ty (Expr expr)
        pure (ty, SUniOp Not e')
  Call s es -> do
    funcs <- gets funcs
    case M.lookup s funcs of
      Nothing -> throwError $ UndefinedSymbol s Func expr
      Just f  -> do
        es' <- mapM checkExpr es
        -- Check that the correct number of arguments was provided
        let nFormals = length (formals f)
            nActuals = length es
        unless (nFormals == nActuals) $ throwError
          (ArgError nFormals nActuals expr)
        -- Check that types of arguments match
        forM_ (zip (map fst es') (map bindType (formals f)))
          $ \(callSite, defSite) ->
              unless (callSite == defSite) $ throwError $ TypeError
                { expected = [defSite]
                , got      = callSite
                , errorLoc = Expr expr
                }
        pure (retType f, SCall s es')
  Dot e field -> do
    fieldName <- case field of
      Var f -> pure f
      _    -> throwError (AccessError field e)

    (t, e') <- checkExpr e
    lval    <- case e' of
      LVal l' -> pure l'
      _       -> throwError (AccessError e field)
    (Struct _ fields) <- case t of
      TypeStruct name' -> do
        ss <- gets structs
        case find (\(Struct n _) -> n == name') ss of
          Nothing -> throwError (TypeError [TypeStruct $ Id "a_struct"] t (Expr expr))
          Just s  -> pure s
      _ -> throwError (TypeError [TypeStruct $ Id "a_struct"] t (Expr expr))
    f <- case findIndex (\(Bind _ f) -> f == fieldName) fields of
      Nothing -> throwError (AccessError e field)
      Just i  -> pure i
    pure (bindType (fields !! f), LVal $ SDot lval f)
  Assign lhs rhs -> do
    lhs'@(t1, _) <- checkExpr lhs
    rhs'@(t2, _) <- checkExpr rhs
    lval         <- case snd lhs' of
      LVal e -> pure e
      _      -> throwError $ AssignmentError lhs rhs
    unless (t1 == t2) $ throwError $ TypeError [t1] t2 (Expr expr)
    pure (t2, SAssign lval rhs')      
  where isNumeric = \case
          TypeInt -> True
          _ -> False
  

checkStatement :: Function -> Stmt -> Semant SStmt
checkStatement func stmt = case stmt of
  Expr e -> SExpr <$> checkExpr e
  If pred cons alt -> do
    pred'@(ty, _) <- checkExpr pred
    unless (ty == TypeBool) $ throwError $ TypeError [TypeBool] ty stmt
    SIf pred' <$> checkStatement func cons <*> checkStatement func alt
  While cond action -> do
    cond'@(ty, _) <- checkExpr cond
    unless (ty == TypeBool) $ throwError $ TypeError [TypeBool] ty stmt
    action' <- checkStatement func action
    pure $ SIf cond' (SDoWhile cond' action') (SBlock []) 
  Return expr -> do
    e@(ty, _) <- checkExpr expr
    unless (ty == retType func) $ throwError $ 
      TypeError [retType func] ty stmt
    pure $ SReturn e
  Block sl -> do
    let flattened = flatten sl
    SBlock <$> mapM (checkStatement func) flattened
   where
    flatten []             = []
    flatten (Block s : ss) = flatten (s ++ ss)
    flatten (s       : ss) = s : flatten ss
    nothingFollowsRet []         = True
    nothingFollowsRet [Return _] = True
    nothingFollowsRet (s : ss  ) = case s of
      Return _ -> False
      _        -> nothingFollowsRet ss     
  PlusPlus e -> do
    e'@(ty, _) <- checkExpr e
    unless (ty == TypeInt) $ throwError $ TypeError [TypeInt] ty stmt
    pure $ SPlusPlus e'
  MinusMinus e -> do
    e'@(ty, _) <- checkExpr e
    unless (ty == TypeInt) $ throwError $ TypeError [TypeInt] ty stmt
    pure $ SMinusMinus e'

checkFunction func = do
  -- add the fname to the table and check for conflicts
  funcs <- gets funcs
  unless (M.notMember (name func) funcs) $ throwError $ Redeclaration (name func)
  -- add this func to symbol table
  modify $ \env -> env { funcs = M.insert (name func) func funcs }

  (formals', locals', body') <- locally $ liftM3
    (,,)
    (checkBinds Formal (F func) (formals func))
    (checkBinds Local (F func) (locals func))
    (checkStatement func (Block $ body func))

  case body' of
    SBlock body'' -> do
      unless (retType func == TypeVoid || validate (genCFG body''))
        $ throwError (TypeError [retType func] TypeVoid (Block $ body func))

      pure $ SFunction { sretType = retType func
                       , sname    = name func
                       , sformals = formals'
                       , slocals  = locals'
                       , sbody    = SBlock body''
                       }
    _ -> error "Internal error - block didn't become a block?" 

checkProgram :: Program -> Either SemantError SProgram
checkProgram program = evalState (runExceptT (checkProgram' program)) baseEnv
 where
  baseEnv = Env { structs = [], vars = M.empty, funcs = M.empty }
  checkProgram' :: Program -> Semant SProgram
  checkProgram' (Program structs binds funcs) = do
    structs' <- mapM checkFields structs
    modify $ \e -> e { structs = structs' }
    globals <- checkBinds Global TopLevel binds
    funcs'  <- mapM checkFunction funcs
    pure (structs', globals, funcs')
