module Exp.Interp.ExpInterp (interp) where

import Data.Char (toLower)

import Exp.Frontend.Frontend
import Exp.Frontend.Typing.TyExp

-- top level interpreter function

interp :: String -> Either String Value
interp inp
  = case frontEnd inp of
      Left err -> Left err
      Right ex -> case eval ex of
                    Just v -> Right v
                    _      -> Left "Impossible! Should result in type error!"

-- value definition and its operations

data Value = VInt Int | VBool Bool

instance Show Value where
  show (VInt n) = show n
  show (VBool b) = map toLower $ show b


(.+.) :: Value -> Value -> Value
(VInt n) .+. (VInt m) = VInt (n + m)
_        .+. _        = error "Impossible! Should result in type error!"

(.-.) :: Value -> Value -> Value
(VInt n) .-. (VInt m) = VInt (n - m)
_        .-. _        = error "Impossible! Should result in type error!"


(.&.) :: Value -> Value -> Value
(VBool b1) .&. (VBool b2) = VBool (b1 && b2)
_          .&. _          = error "Impossible! Should result in type error!"

vnot :: Value -> Value
vnot (VBool b) = VBool (not b)
vnot _         = error "Impossible! Should result in type error!"

vIsZero :: Value -> Value
vIsZero (VInt v) = VBool (v == 0)
vIsZero _        = error "Impossible! Should result in type error!"


-- evaluation function

eval :: TyExp -> Maybe Value
eval (_, TInt n) = Just (VInt n)
eval (_, TBool b) = Just (VBool b)
eval (_, TAdd e1 e2)
  = (.+.) <$> eval e1 <*> eval e2
eval (_, TSub e1 e2)
  = (.-.) <$> eval e1 <*> eval e2
eval (_, TAnd e1 e2)
  = (.&.) <$> eval e1 <*> eval e2
eval (_, TNot e1)
  = vnot <$> eval e1
eval (_, TIsZero e1)
  = vIsZero <$> eval e1
eval (_, TIf e1 e2 e3)
  = do
       v1 <- eval e1
       case v1 of
         VBool True -> eval e2
         VBool False -> eval e3
         _           -> error "Impossible! Should result in type error!"
