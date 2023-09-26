module Imp.Backend.Backend (backend) where

import Imp.Backend.Stack.StatementCodegen
import Imp.Frontend.Syntax.Statement
import Imp.Pipeline.OptionParser
import Stack.Frontend.Syntax.Instr

backend :: Backend -> Program String -> Maybe Output -> IO ()
backend VM ast out = compileVM ast out
backend LLVM _ _ = undefined
backend _ _ _ = error "Impossible!"

compileVM :: Program String -> Maybe Output -> IO ()
compileVM ast out
  = do
      code <- compileProgram ast
      let pcode = pprintCode code
      case out of
        Just (Output file) -> writeFile file pcode 
        Nothing -> putStrLn (pprintCodeD code)
