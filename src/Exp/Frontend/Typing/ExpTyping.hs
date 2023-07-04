module Exp.Frontend.Typing.ExpTyping (typeCheck, Ty, TypeError) where

import Control.Monad.Except
import Data.Functor.Identity

import Exp.Frontend.Syntax.ExpSyntax
import Exp.Frontend.Typing.TyExp

-- auxiliar definitions for type checking

type Expected = Ty
type Found = Ty

data TypeError = TypeError Expected Found deriving Show

type TcM a = (ExceptT TypeError Identity) a

-- main type checking algorithm

typeCheck' :: Exp -> TcM TyExp
typeCheck' (EInt n) = return (Int, TInt n)
typeCheck' (EBool b) = return (Bool, TBool b)
typeCheck' (e1 :+: e2)
  = do
       e1'@(t1, _) <- typeCheck' e1
       e2'@(t2, _) <- typeCheck' e2
       if t1 == t2 then
         if t1 == Int then return (Int, TAdd e1' e2')
         else throwError (TypeError Int t1)
       else throwError (TypeError t1 t2)
typeCheck' (e1 :-: e2)
  = do
       e1'@(t1, _) <- typeCheck' e1
       e2'@(t2, _) <- typeCheck' e2
       if t1 == t2 then
         if t1 == Int then return (Int, TSub e1' e2')
         else throwError (TypeError Int t1)
       else throwError (TypeError t1 t2)
typeCheck' (e1 :&: e2)
  = do
       e1'@(t1, _) <- typeCheck' e1
       e2'@(t2, _) <- typeCheck' e2
       if t1 == t2 then
         if t1 == Bool then return (Bool, TAnd e1' e2')
         else throwError (TypeError Bool t1)
       else throwError (TypeError t1 t2)
typeCheck' (Not e)
  = do
      e'@(t, _) <- typeCheck' e
      if t == Bool then return (Bool, TNot e')
        else throwError (TypeError t Bool)
typeCheck' (IsZero e)
  = do
      e'@(t, _) <- typeCheck' e
      if t == Int then return (Bool, TIsZero e')
        else throwError (TypeError t Int)
typeCheck' (If e1 e2 e3)
  = do
      e1'@(t1, _) <- typeCheck' e1
      e2'@(t2, _) <- typeCheck' e2
      e3'@(t3, _) <- typeCheck' e3
      if t1 == Bool then
        if t2 == t3 then return (t2, TIf e1' e2' e3')
        else throwError (TypeError t2 t3)
      else throwError (TypeError t1 Bool)

-- top level type checker function

typeCheck :: Exp -> Either String TyExp
typeCheck e
  = case runIdentity (runExceptT (typeCheck' e)) of
      Left (TypeError f ex) ->
        Left $ "Type Error!\nFound:" ++ show f ++ "\nExpected:" ++ show ex
      Right t -> Right t
