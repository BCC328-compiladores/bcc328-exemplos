module Exp.Frontend.Typing.TyExp ( Ty (..)
                                 , TyExp
                                 , TyExp' (..)
                                 ) where

-- definition of types

data Ty = Int | Bool deriving (Eq, Ord, Show)

-- typed expressions: used as result for the typechecker

type TyExp = (Ty, TyExp')

data TyExp'
  = TInt Int
  | TBool Bool
  | TAdd TyExp TyExp
  | TSub TyExp TyExp
  | TIsZero TyExp
  | TAnd TyExp TyExp
  | TNot TyExp
  | TIf TyExp TyExp TyExp
  deriving (Eq, Ord, Show)
