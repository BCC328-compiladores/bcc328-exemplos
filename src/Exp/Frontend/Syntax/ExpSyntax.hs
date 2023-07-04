module Exp.Frontend.Syntax.ExpSyntax (Exp (..)) where

-- definition of simple expressions

data Exp
  = EInt Int
  | EBool Bool
  | Exp :+: Exp
  | Exp :-: Exp
  | IsZero Exp
  | Exp :&: Exp
  | Not Exp
  | If Exp Exp Exp
  deriving (Eq, Ord, Show)
