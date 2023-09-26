module Imp.Backend.Stack.ExprCodegen ( CompileM
                                     , compileExpr
                                     , lookupVar
                                     , runCompileM
                                     ) where

import Control.Monad.State
import qualified Data.Map as Map

import Imp.Frontend.Syntax.Exp
import Stack.Frontend.Syntax.Instr

type Env = Map.Map String Int
type CompileM a = (StateT GenState IO) a


data GenState
  = GenState {
      env :: Env
    , varCounter :: Int
    }

emptyGenState :: GenState
emptyGenState = GenState Map.empty 0

lookupVar :: Var String -> CompileM Int
lookupVar (Var s)
  = do
       e <- gets env
       case Map.lookup s e of
         Just v -> return v
         Nothing ->
           do
             n <- gets varCounter
             modify (\ st -> st{ varCounter = (varCounter st) + 1
                               , env = Map.insert s n e })
             return n

runCompileM :: CompileM a -> IO a
runCompileM m = fst <$> runStateT m emptyGenState

-- definition of the code generator

compileValue :: Value -> CompileM Code
compileValue (EInt n) = return [PUSHI n]
compileValue (EBool b) = return [PUSHI (if b then 1 else 0)]

compileBinOp :: Instr -> Exp String -> Exp String -> CompileM Code
compileBinOp f e1 e2
  = do
       c1 <- compileExpr e1
       c2 <- compileExpr e2
       return $ c2 ++ c1 ++ [f]

compileExpr :: Exp String -> CompileM Code
compileExpr (EValue v)
  = compileValue v
compileExpr (EVar v)
  = do
      addr <- lookupVar v
      return [PUSHI addr, LOAD]
compileExpr (e1 :+: e2)
  = compileBinOp ADD e1 e2
compileExpr (e1 :*: e2)
  = compileBinOp MUL e1 e2
compileExpr (e1 :-: e2)
  = compileBinOp SUB e2 e1
compileExpr (e1 :/: e2)
  = compileBinOp DIV e2 e1
compileExpr (e1 :==: e2)
  = compileBinOp IEQ e1 e2
compileExpr (e1 :<: e2)
  = compileBinOp ILT e2 e1
compileExpr (e1 :&: e2)
  = compileBinOp AND e1 e2
compileExpr (ENot e)
  = ((++) [NOT]) <$> compileExpr e


