import Data.Maybe (fromMaybe)
import Markup.Printer.Html (Head, render, title_)
import Markup.Language.Frontend
import Markup.Printer.Backend
import Options.Applicative
import System.Directory (doesFileExist)
import System.Exit (exitFailure, exitSuccess)
import System.IO


main :: IO ()
main = do
  options <- parse
  (title,inpHandle,outHandle) <- createHandles options
  startPipeline title inpHandle outHandle
  hClose inpHandle
  hClose outHandle

createHandles :: Options -> IO (String, Handle, Handle)
createHandles (Single inp out)
  = do
      (title, inpHandle) <- createInputHandle inp
      outHandle <- createOutputHandle out
      return (title, inpHandle, outHandle)

createInputHandle :: Input -> IO (String, Handle)
createInputHandle Stdin = return ("", stdin)
createInputHandle (FileInput path)
  = (,) path <$> openFile path ReadMode

confirmOverwrite :: FilePath -> IO Bool
confirmOverwrite path
  = do
      putStrLn $ "File:\n" ++ path ++ "\nexists. Confirm overwrite?"
      answer <- getLine
      case answer of
        "y" -> return True
        "n" -> return False
        _   -> do
                 putStrLn "Invalid answer! Please type y or n."
                 confirmOverwrite path

createOutputHandle :: Output -> IO Handle
createOutputHandle Stdout = return stdout
createOutputHandle (FileOutput path)
  = do
      exists <- doesFileExist path
      canWrite <- if exists then confirmOverwrite path
                  else return True
      if canWrite then openFile path WriteMode
        else exitSuccess

---------------------------------------------------
-- compilation pipeline                          --
---------------------------------------------------

startPipeline :: String -> Handle -> Handle -> IO ()
startPipeline title inpHandle outHandle
  = do
      content <- hGetContents inpHandle
      res <- pipeline (title_ title) content
      hPutStrLn outHandle res

pipeline :: Head -> String -> IO String
pipeline title content
  = case frontEnd content of
      Left err -> putStrLn err >> exitFailure
      Right ast -> return $ render $ backEnd title ast

----------------------------------------------------
-- definition of the command line argument parser --
----------------------------------------------------

-- basic types for options 

data Input
  = Stdin              -- input from console
  | FileInput FilePath -- file name
  deriving Show 

data Output
  = Stdout                -- output to console 
  | FileOutput FilePath   -- file name
  deriving Show
 
data Options
  = Single Input Output
  deriving Show

-- functions for defining the parser

parse :: IO Options
parse = execParser opts

opts :: ParserInfo Options
opts = info (pSingle <**> helper)
            (fullDesc                            <>
             header "BCC328 - Markdown compiler" <>
             progDesc "Simplified Markdown compiler")



pSingle :: Parser Options
pSingle =
  Single <$> pSingleInput <*> pSingleOutput

pSingleInput :: Parser Input
pSingleInput =
  fromMaybe Stdin <$> optional pInputFile

pSingleOutput :: Parser Output
pSingleOutput =
  fromMaybe Stdout <$> optional pOutputFile

pInputFile :: Parser Input
pInputFile = FileInput <$> parser
  where
    parser =
      strOption
        ( long "input"
          <> short 'i'
          <> metavar "FILE"
          <> help "Input file"
        )

pOutputFile :: Parser Output
pOutputFile = FileOutput <$> parser
  where
    parser =
      strOption
        ( long "output"
          <> short 'o'
          <> metavar "FILE"
          <> help "Output file"
        )
