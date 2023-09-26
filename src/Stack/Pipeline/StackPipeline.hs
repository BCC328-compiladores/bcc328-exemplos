module Stack.Pipeline.StackPipeline (startPipeline) where

import Stack.Frontend.Parser.StackParser
import Stack.Pipeline.OptionsParser
import Stack.Backend.Semantics.Interp

startPipeline :: IO ()
startPipeline
  = do 
      Option (Input inpFile) <- optionsParser
      content <- readFile inpFile
      case parseCode content of
        Left err -> putStrLn err
        Right cs -> execute cs 

execute :: Code -> IO ()
execute cs
  = do
      res <- runVM cs
      case res of
        Left err1 -> print err1
        Right _ -> return ()
