module Exp.Frontend.Frontend (frontEnd) where

import Exp.Frontend.Parser.ExpParser
import Exp.Frontend.Typing.ExpTyping
import Exp.Frontend.Typing.TyExp

frontEnd :: String -> Either String TyExp
frontEnd inp
  = case expParser inp of
      Left err -> Left err
      Right e -> typeCheck e
