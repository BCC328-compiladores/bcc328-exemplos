module Arith.Frontend.Syntax.Arith (Arith (..)) where

-- simple arithmetic language

data Arith
  = Const Int
  | Arith :+: Arith
  | Arith :*: Arith
  deriving (Eq, Ord, Show)

