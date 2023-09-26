module Imp.Interpreter.Semantics.StatementSemantics (semanticsProgram) where

import Control.Monad.Trans
import Control.Monad.Except
import Text.Megaparsec

import Imp.Frontend.Parser.ExpParsecParser
import Imp.Frontend.Pretty.ExpPretty
import Imp.Frontend.Syntax.Exp
import Imp.Frontend.Syntax.Statement
import Imp.Frontend.Typing.Ty
import Imp.Interpreter.Semantics.Monad
import Imp.Interpreter.Semantics.ExpSemantics

defaultValue :: Ty -> Value
defaultValue TInt = EInt 0
defaultValue TBool = EBool False

semanticsProgram :: Program String -> IO ()
semanticsProgram ss = runExecM (semanticsBlock ss)

semanticsBlock :: Block String -> ExecM ()
semanticsBlock (Block blk)
  = do
      vs <- concat <$> mapM semanticsStmt blk
      removeEnv vs

semanticsStmt :: Stmt String -> ExecM [Var String]
semanticsStmt Skip = return []
semanticsStmt (Def ty v e)
  = do
      val <- maybe (return $ defaultValue ty) semanticsExpr e
      insertEnv v val
      return [v]
semanticsStmt (v := e)
  = do
      val <- semanticsExpr e
      insertEnv v val
      return []
semanticsStmt (If e blk1 blk2)
  = do
      val <- semanticsExpr e
      if val == EBool True then semanticsBlock blk1 
        else semanticsBlock blk2
      return []
semanticsStmt (Print e)
  = do
      val <- semanticsExpr e
      liftIO (putStr (render (pprint val)))
      return []
semanticsStmt (SRead v)
  = do
       inp <- liftIO getLine
       case parse parserValue "" inp of
         Left _ -> throwError "invalid input!"
         Right val -> insertEnv v val >> return []
semanticsStmt (While e blk)
  = do
      val <- semanticsExpr e
      if val == EBool True then do
        semanticsBlock blk
        semanticsStmt (While e blk)
      else return []
