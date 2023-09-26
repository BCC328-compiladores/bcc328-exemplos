module Imp.Frontend.Parser.ExpParsecParser ( parserExpr
                                           , parserVar
                                           , parserValue
                                           ) where

import Control.Monad.Combinators
import Control.Monad.Combinators.Expr

import Imp.Frontend.Lexer.ImpParsecLexer
import Imp.Frontend.Syntax.Exp


-- definition of a parser for values

pInt :: Parser Int
pInt = int

pBool :: Parser Bool
pBool = False <$ rword "false" <|> True <$ rword "true"

parserValue :: Parser Value
parserValue = EInt <$> pInt <|> EBool <$> pBool

-- parsing variables

identifier :: Parser String
identifier = lexeme ident
  where
    ident = (:) <$> letter <*> many alphaNum

parserVar :: Parser (Var String)
parserVar = Var <$> identifier

-- terms are variables or values

parserTerm :: Parser (Exp String)
parserTerm = pVal <|> (parens parserExpr) <|> pVar
  where
    pVal = EValue <$> parserValue
    pVar = EVar <$> parserVar

-- parser for expressions

parserExpr :: Parser (Exp String)
parserExpr = makeExprParser parserTerm table
  where
    pprefix s f = Prefix (f <$ symbol s)
    pinfixl s f = InfixL (f <$ symbol s)
    pinfixn s f = InfixN (f <$ symbol s)
    table = [ [ pinfixl "+" (:+:)
              , pinfixl "-" (:-:)]
            , [ pinfixl "*" (:*:)
              , pinfixl "/" (:/:)]
            , [ pinfixn "==" (:==:)
              , pinfixn "<" (:<:)]
            , [ pinfixl "&&" (:&:)
              , pprefix "!" ENot]
            ]
