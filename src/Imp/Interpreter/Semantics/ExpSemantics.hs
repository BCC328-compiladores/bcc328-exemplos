module Imp.Interpreter.Semantics.ExpSemantics (semanticsExpr) where

import Imp.Interpreter.Semantics.Monad
import Imp.Frontend.Syntax.Exp

(.+.) :: Value -> Value -> Value
(EInt n) .+. (EInt m) = EInt (n + m)
_ .+. _ = error "Impossible! Type Error"

(.-.) :: Value -> Value -> Value
(EInt n) .-. (EInt m) = EInt (n - m)
_ .-. _ = error "Impossible! Type Error"

(.*.) :: Value -> Value -> Value
(EInt n) .*. (EInt m) = EInt (n * m)
_ .*. _ = error "Impossible! Type Error"

(./.) :: Value -> Value -> Value
(EInt n) ./. (EInt m) = EInt (div n m)
_ ./. _ = error "Impossible! Type Error"

semanticsExpr :: Exp String -> ExecM Value
semanticsExpr (EValue v) = return v
semanticsExpr (EVar v) = lookupEnv v
semanticsExpr (e1 :+: e2)
  = (.+.) <$> semanticsExpr e1 <*> semanticsExpr e2
semanticsExpr (e1 :-: e2)
  = (.-.) <$> semanticsExpr e1 <*> semanticsExpr e2
semanticsExpr (e1 :*: e2)
  = (.*.) <$> semanticsExpr e1 <*> semanticsExpr e2
semanticsExpr (e1 :/: e2)
  = (./.) <$> semanticsExpr e1 <*> semanticsExpr e2
semanticsExpr (e1 :==: e2)
  = f <$> semanticsExpr e1 <*> semanticsExpr e2
    where
      f x y = EBool (x == y)
semanticsExpr (e1 :<: e2)
  = f <$> semanticsExpr e1 <*> semanticsExpr e2
    where
      f x y = EBool (x < y)
semanticsExpr (ENot e)
  = f <$> semanticsExpr e
    where
      f (EBool v) = EBool (not v)
      f _ = error "Impossible! Type error!"
semanticsExpr (e1 :&: e2)
  = f <$> semanticsExpr e1 <*> semanticsExpr e2
    where
      f (EBool v1) (EBool v2) = EBool (v1 && v2)
      f _ _ = error "Impossible! Type error!"
