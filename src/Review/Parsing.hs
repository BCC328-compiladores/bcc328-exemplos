module Review.Parsing (parser, symbol, parse, eof) where

import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

-- representação da AST

data AST
  = Zero AST AST -- starts with 0
  | One AST AST  -- starts with 1  
  | Empty        -- empty string
  deriving Show

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
 where
  lineCmnt  = L.skipLineComment "--"
  blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = lexeme . L.symbol sc

startWithZero :: Parser AST
startWithZero
  = f <$> symbol "0" <*> sample <*> symbol "1"  <*> sample
    where
      f _ t _ t' = Zero t t'

startWithOne :: Parser AST
startWithOne
  = g <$> symbol "1" <*> sample <*> symbol "0"  <*> sample
    where
      g _ t _ t' = One t t'

sample :: Parser AST
sample = (try startWithZero) <|>
         (try startWithOne)  <|>
         (pure Empty)

parser :: String -> Either String AST
parser s = case parse (sample <* eof) "" s of
            Left err -> Left (errorBundlePretty err)
            Right t -> Right t
