module Imp.Pipeline.OptionParser ( optionsParser
                                 , Backend (..)
                                 , Input (..)
                                 , Output (..)
                                 , Option (..)
                                 ) where

import Options.Applicative

-- type for Options

data Option
  = Option Backend Input (Maybe Output)
  deriving Show

-- type for specifying lexer

data Lexer = LexerParsec
  deriving Show

-- type for specifying parser

data Parsing = ParserParsec deriving Show

-- type for specifying the backend

data Backend
  = LLVM
  | VM
  | Interpreter
  deriving Show

-- type for input file

data Input = Input FilePath deriving Show
data Output = Output FilePath deriving Show

-- top level parsing function

optionsParser :: IO Option
optionsParser = execParser opts

-- parser for options

opts :: ParserInfo Option
opts = info (pOption <**> helper)
            (fullDesc <>
             header "BCC328 - Imp compiler" <>
             progDesc "Sample Imp compiler for Compiler Lectures on UFOP.")

pOption :: Parser Option
pOption
  = Option <$> pBackend       <*>
               pInputFile     <*>
               pOutputFile 

-- parser for backend parameter

pBackend :: Parser Backend
pBackend = option pBack
                  (  long "backend"
                  <> value LLVM
                  <> help "Specify the backend or interpreter mode.")
  where
    pBack = do
      s <- str
      case s of
        "llvm" -> return LLVM
        "vm"   -> return VM
        "interpreter" -> return Interpreter
        _ -> readerError "Valid options are: llvm, vm, intepreter."


-- parser for input file

pInputFile :: Parser Input
pInputFile = Input <$> parser
  where
    parser =
      strOption
        ( long "input"
          <> short 'i'
          <> metavar "FILE"
          <> help "Input file"
        )

-- parser for output file

pOutputFile :: Parser (Maybe Output)
pOutputFile = optional (Output <$> parser)
  where
    parser =
      strOption
        ( long "output"
          <> short 'o'
          <> metavar "FILE"
          <> help "Output file"
        )
