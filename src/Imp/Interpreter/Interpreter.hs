module Imp.Interpreter.Interpreter (interpreter) where

import Imp.Frontend.Parser.StatementParsecParser
import Imp.Frontend.Typing.StatementTyping
import Imp.Interpreter.Semantics.StatementSemantics


interpreter :: FilePath -> IO ()
interpreter program
  = do
       content <- readFile program
       case parseProgram content of
         Left err -> putStrLn err
         Right ast ->
           case typeCheck ast of
             Left err' -> putStrLn err'
             Right _   -> semanticsProgram ast
