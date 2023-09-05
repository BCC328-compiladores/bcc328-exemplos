module Arith.Interp.ArithInterp (eval) where

import Arith.Frontend.Syntax.Arith

eval :: Arith -> Int
eval (Const n) = n
eval (e1 :+: e2) = eval e1 + eval e2
eval (e1 :*: e2) = eval e1 * eval e2

