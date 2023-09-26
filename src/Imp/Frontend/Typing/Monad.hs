module Imp.Frontend.Typing.Monad ( TcM
                                 , runTcM
                                 , varNotFound
                                 , incompatibleTypes
                                 , lookupVar
                                 , insertVar
                                 , removeVars
                                 , module Control.Monad.State
                                 ) where

import Control.Monad.Except
import Control.Monad.State
import Data.Functor.Identity

import Imp.Frontend.Syntax.Exp
import Imp.Frontend.Typing.Ty


-- type for representing possible errors

type Expected = Ty
type Found = Ty

data TypingErr
  = VarNotFound (Var String) 
  | IncompatibleTypes Expected Found 

instance Show TypingErr where
  show (VarNotFound v) = "Undefined variable: " ++ show v
  show (IncompatibleTypes ex fd) = "Incompatible types:\n" ++
                                   "Expected:" ++ show ex ++ "\n" ++
                                   "Found:" ++ show fd ++ "\n"

-- definition of the type checking monad

type TcM a = ExceptT TypingErr (StateT Ctx Identity) a

lookupVar :: Var String -> TcM Ty
lookupVar v
  = do
      t <- gets (lookupCtx v)
      maybe (varNotFound v) return t

insertVar :: Var String -> Ty -> TcM ()
insertVar v ty = modify (insertCtx v ty)

removeVars :: [Var String] -> TcM ()
removeVars vs
  = modify (\ ctx -> foldr removeCtx ctx vs)

varNotFound :: Var String -> TcM a
varNotFound = throwError . VarNotFound

incompatibleTypes :: Expected -> Found -> TcM a
incompatibleTypes e f = throwError (IncompatibleTypes e f)

runTcM :: Ctx -> TcM a -> Either TypingErr a
runTcM ctx tc = fst $ runIdentity (runStateT (runExceptT tc) ctx)
