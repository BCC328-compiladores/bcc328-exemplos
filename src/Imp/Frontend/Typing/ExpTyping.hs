module Imp.Frontend.Typing.ExpTyping (tcExp) where

import Control.Monad

import Imp.Frontend.Typing.Monad
import Imp.Frontend.Typing.Ty
import Imp.Frontend.Syntax.Exp


tcExp :: Exp String -> TcM Ty
tcExp (EValue v) = tcValue v
tcExp (EVar v) = lookupVar v
tcExp (e1 :+: e2)
  = tcBinOp (e1, TInt) (e2, TInt) TInt
tcExp (e1 :-: e2)
  = tcBinOp (e1, TInt) (e2, TInt) TInt
tcExp (e1 :*: e2)
  = tcBinOp (e1, TInt) (e2, TInt) TInt
tcExp (e1 :/: e2)
  = tcBinOp (e1, TInt) (e2, TInt) TInt
tcExp (e1 :==: e2)
  = tcBinOp (e1, TInt) (e2, TInt) TBool
tcExp (e1 :<: e2)
  = tcBinOp (e1, TInt) (e2, TInt) TBool
tcExp (ENot e1)
  = do
      t1 <- tcExp e1
      unless (t1 == TBool) (incompatibleTypes TBool t1)
      return TBool
tcExp (e1 :&: e2)
  = tcBinOp (e1, TBool) (e2, TBool) TBool

tcBinOp :: (Exp String, Ty) -> (Exp String, Ty) -> Ty -> TcM Ty
tcBinOp (e1, te1) (e2, te2) ter
  = do
       tf1 <- tcExp e1
       tf2 <- tcExp e2
       unless (tf1 == te1) (incompatibleTypes te1 tf1)
       unless (tf2 == te2) (incompatibleTypes te2 tf2)
       return ter

tcValue :: Value -> TcM Ty
tcValue (EInt _) = return TInt
tcValue (EBool _) = return TBool
