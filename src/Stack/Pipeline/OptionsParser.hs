module Stack.Pipeline.OptionsParser ( optionsParser
                                    , Option (..)
                                    , Input (..)
                                    ) where

import Options.Applicative

-- type for Options

data Option
  = Option Input 
  deriving Show

-- type for input file

data Input = Input FilePath deriving Show

-- top level parsing function

optionsParser :: IO Option
optionsParser = execParser opts

-- parser for options

opts :: ParserInfo Option
opts = info (pOption <**> helper)
            (fullDesc <>
             header "BCC328 - Stack Virtual Machine Interpreter" <>
             progDesc "Sample Stack-based virtual machine for Compiler Lectures on UFOP.")

pOption :: Parser Option
pOption
  = Option <$> pInputFile

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
