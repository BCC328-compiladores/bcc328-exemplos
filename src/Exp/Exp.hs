import Exp.Frontend.Frontend
import Exp.Backend.Backend
import Exp.Interp.ExpInterp
import Options.Applicative
import System.Console.Haskeline

-- command line argument parser

data Input
  = FileInput FilePath -- uses direct compilation
  | Repl               -- evaluate expressions using the REPL


fileInputP :: Parser Input
fileInputP = FileInput <$> strOption
  (  long "file"
  <> short 'f'
  <> metavar "FILENAME"
  <> help "Input file" )

replP :: Parser Input
replP = flag' Repl
        (  long "repl"
        <> help "Starts the interpreter repl")

input :: Parser Input
input = fileInputP <|> replP

opts :: ParserInfo Input
opts = info input
       (fullDesc
       <> header "Arithmetic Expression interpreter and compiler")

-- repl definition

repl :: IO ()
repl = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "exp> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just "quit" -> outputStrLn "Goodbye."
      Just inp ->
        case interp inp of
          Left err -> outputStrLn err >> loop
          Right v  -> outputStrLn (show v) >> loop


-- main expression function.

startPipeline :: Input -> IO ()
startPipeline (FileInput fname)
  = do
      prog <- readFile fname
      case frontEnd prog of
        Left err -> putStrLn err
        Right e  -> backEnd fname e
startPipeline Repl
  = repl


-- definition of main function

main :: IO ()
main = execParser opts >>= startPipeline
