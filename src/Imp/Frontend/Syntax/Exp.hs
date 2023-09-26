module Imp.Frontend.Syntax.Exp ( Exp (..)
                               , Var (..)
                               , Value (..)
                               ) where

-- definition of expressions

newtype Var a = Var { unVar :: a } deriving (Eq, Ord, Show)

data Value
  = EInt Int
  | EBool Bool
  deriving (Eq, Ord, Show)

data Exp a
  = EValue Value 
  | EVar (Var a)
  | (Exp a) :+: (Exp a)
  | (Exp a) :*: (Exp a)
  | (Exp a) :-: (Exp a)
  | (Exp a) :/: (Exp a)
  | (Exp a) :==: (Exp a)
  | (Exp a) :<: (Exp a)
  | ENot (Exp a)
  | (Exp a) :&: (Exp a)
  deriving (Eq, Ord, Show)
