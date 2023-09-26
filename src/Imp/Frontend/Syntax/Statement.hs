module Imp.Frontend.Syntax.Statement ( Stmt (..)
                                     , Block (..)
                                     , Program
                                     ) where

import Imp.Frontend.Typing.Ty (Ty)
import Imp.Frontend.Syntax.Exp

-- syntax for statements

type Program a = Block a

newtype Block a
  = Block { unBlock :: [Stmt a] }
    deriving (Eq, Ord, Show)

data Stmt a
  = Skip
  | Def Ty (Var a) (Maybe (Exp a))
  | (Var a) := (Exp a)
  | If (Exp a) (Block a) (Block a)
  | Print (Exp a)
  | SRead (Var a)
  | While (Exp a) (Block a)
  deriving (Eq, Ord, Show)
