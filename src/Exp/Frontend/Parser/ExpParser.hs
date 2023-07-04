module Exp.Frontend.Parser.ExpParser (expParser) where

import Exp.Frontend.Syntax.ExpSyntax
import Exp.Frontend.Lexer.ExpLexer

import Text.Megaparsec
import Control.Monad.Combinators.Expr
import Control.Applicative (liftA3)

-- definition of operator table

opTable :: [[Operator Parser Exp]]
opTable
  = [ [ unary Not "!" ]
    , [ infixL (:+:) "+"
      , infixL (:-:) "-"
      ]
    , [ infixL (:&:) "&&" ]
    ]
 where
  unary op sym = Prefix $ foldr1 (.) <$> some (op <$ symbol sym)
  infixL op sym = InfixL $ op <$ symbol sym

termP :: Parser Exp
termP = parens expP
    <|> EInt <$> int
    <|> EBool True <$ rword "true"
    <|> EBool False <$ rword "false"
    <|> ifP

ifP :: Parser Exp
ifP = liftA3 If (rword "if" *> expP) (rword "then" *> expP) (rword "else" *> expP)

expP :: Parser Exp
expP = makeExprParser termP opTable

expParser :: String -> Either String Exp
expParser s = case parse expP "" s of
                Left err -> Left (errorBundlePretty err)
                Right e  -> Right e
