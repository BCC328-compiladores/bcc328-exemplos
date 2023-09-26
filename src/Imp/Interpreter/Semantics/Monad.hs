module Imp.Interpreter.Semantics.Monad ( Env
                                       , ExecM
                                       , runExecM
                                       , insertEnv
                                       , lookupEnv
                                       , removeEnv
                                       ) where


import Control.Monad.Except
import Control.Monad.State
import Data.Map hiding (foldr)
import Data.Maybe

import Prelude hiding (lookup)

import Imp.Frontend.Syntax.Exp


-- definition of the semantics environment

type Env = Map (Var String) Value

insertEnv :: Var String -> Value -> ExecM ()
insertEnv v val = modify (insert v val)

lookupEnv :: Var String -> ExecM Value
lookupEnv v
  = do
       val <- gets (lookup v)
       case val of
         Nothing -> throwError $ "Variable undefined:" ++ (unVar v)
         Just val' -> return val'

removeEnv :: [Var String] -> ExecM ()
removeEnv vs = modify (\ env -> foldr delete env vs)

-- definition of the monad

type ExecM a = ExceptT String (StateT Env IO) a

runExecM :: ExecM a -> IO ()
runExecM m
  = do
       res <- runStateT (runExceptT m) empty
       case fst res of
         Left err -> putStrLn err
         Right _  -> return ()
