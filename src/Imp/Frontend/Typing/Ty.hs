module Imp.Frontend.Typing.Ty ( Ctx
                              , Ty (..)
                              , lookupCtx
                              , insertCtx
                              , removeCtx
                              ) where

import Data.Map (Map, lookup, insert, delete)
import Prelude hiding (lookup)

import Imp.Frontend.Syntax.Exp (Var)

type Ctx = Map (Var String) Ty

data Ty = TInt | TBool deriving (Eq, Ord)

instance Show Ty where
  show TInt = "int"
  show TBool = "bool"

lookupCtx :: Var String -> Ctx -> Maybe Ty
lookupCtx v ctx = lookup v ctx

insertCtx :: Var String -> Ty -> Ctx -> Ctx
insertCtx v t ctx = insert v t ctx

removeCtx :: Var String -> Ctx -> Ctx
removeCtx v ctx = delete v ctx
