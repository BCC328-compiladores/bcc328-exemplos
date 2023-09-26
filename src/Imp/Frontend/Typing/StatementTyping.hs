module Imp.Frontend.Typing.StatementTyping (typeCheck) where

import Control.Monad

import Data.Map (empty)

import Imp.Frontend.Typing.Monad
import Imp.Frontend.Typing.Ty
import Imp.Frontend.Typing.ExpTyping

import Imp.Frontend.Syntax.Exp
import Imp.Frontend.Syntax.Statement

typeCheck :: Program String -> Either String (Program String)
typeCheck blk
  = case runTcM empty (tcBlock blk) of
      Left err -> Left $ show err
      Right _  -> Right blk

tcBlock :: Block String -> TcM ()
tcBlock (Block blk)
  = do
      vs <- concat <$> mapM tcStmt blk
      removeVars vs


tcStmt :: Stmt String -> TcM [Var String]
tcStmt Skip = return []
tcStmt (Def ty v e)
  = case e of
      Nothing -> return [v]
      Just e' -> do
        ty' <- tcExp e'
        unless (ty == ty') (incompatibleTypes ty ty')
        insertVar v ty
        return [v]
tcStmt (v := e)
  = do
      ty <- lookupVar v
      ty' <- tcExp e
      unless (ty == ty') (incompatibleTypes ty ty')
      return []
tcStmt (If e blk1 blk2)
  = do
      ty <- tcExp e
      unless (ty == TBool) (incompatibleTypes TBool ty)
      tcBlock blk1
      tcBlock blk2
      return []
tcStmt (Print e)
  = do
      _ <- tcExp e
      return []
tcStmt (SRead v)
  = do
      _ <- lookupVar v
      return []
tcStmt (While e blk)
  = do
      ty <- tcExp e
      unless (ty == TBool) (incompatibleTypes TBool ty)
      tcBlock blk
      return []
