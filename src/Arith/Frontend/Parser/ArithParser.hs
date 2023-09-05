module Arith.Frontend.Parser.ArithParser (arithParser) where

import Arith.Syntax.ArithSyntax
import Arith.Lexer.ArithLexer

import Text.Megaparsec
import Control.Monad.Combinators.Expr
import Control.Applicative (liftA3)

-- definition of operator table

opTable :: [[Operator Parser Exp]]
opTable
  = [ 
      [ infixL (:+:) "+" ]
    , [ infixL (:*:) "*" ]
    ]
 where
  infixL op sym = InfixL $ op <$ symbol sym

termP :: Parser Exp
termP = parens expP
    <|> EInt <$> int

arithP :: Parser Exp
arithP = makeExprParser termP opTable

arithParser :: String -> Either String Exp
arithParser s = case parse arithP "" s of
                Left err -> Left (errorBundlePretty err)
                Right e  -> Right e
