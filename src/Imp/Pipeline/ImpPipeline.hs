module Imp.Pipeline.ImpPipeline ( startPipeline ) where

import Imp.Frontend.Frontend
import Imp.Backend.Backend
import Imp.Interpreter.Interpreter
import Imp.Pipeline.OptionParser


startPipeline :: IO ()
startPipeline
  = do
  options <- optionsParser
  case options of
    Option back inpFile outFile ->
      startCompiler back inpFile outFile


startCompiler :: Backend -> Input -> Maybe Output -> IO ()
startCompiler Interpreter (Input file) _
  = interpreter file
startCompiler b (Input file) outfile
  = do
       content <- readFile file 
       case frontend content of
         Left err -> putStrLn err
         Right ast -> backend b ast outfile
