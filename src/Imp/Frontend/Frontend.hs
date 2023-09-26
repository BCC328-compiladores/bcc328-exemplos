module Imp.Frontend.Frontend (frontend) where

import Imp.Frontend.Parser.StatementParsecParser
import Imp.Frontend.Syntax.Statement
import Imp.Frontend.Typing.StatementTyping

frontend :: String -> Either String (Program String)
frontend code
  = case parseProgram code of
      Left err -> Left err
      Right prog -> typeCheck prog
