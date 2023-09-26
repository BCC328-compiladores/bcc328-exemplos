module Imp.Frontend.Parser.StatementParsecParser (parseProgram) where

import Imp.Frontend.Parser.ExpParsecParser
import Imp.Frontend.Lexer.ImpParsecLexer
import Imp.Frontend.Syntax.Exp
import Imp.Frontend.Syntax.Statement
import Imp.Frontend.Typing.Ty

import Control.Applicative.Combinators
import Text.Megaparsec hiding (many)



parserTy :: Parser Ty
parserTy
  = TInt  <$ rword "int"<|>
    TBool <$ rword "bool" 


parserSemi :: Parser ()
parserSemi = () <$ symbol ";"

parserSkip :: Parser (Stmt String)
parserSkip = Skip <$ rword "skip" <* parserSemi


parserAssignment :: Parser (Stmt String)
parserAssignment
  = f <$> parserVar <*> symbol ":=" <*> parserExpr <* parserSemi
  where
    f v _ e = v := e

parserInitializer :: Parser (Maybe (Exp String))
parserInitializer
  = (Just <$> (rword ":=" *> parserExpr)) <|> return Nothing

parserVarDef :: Parser (Stmt String)
parserVarDef
  = Def <$> parserTy <*> parserVar <*> parserInitializer <* parserSemi

parserBlock :: Bool -> Parser (Block String)
parserBlock False = Block <$> many parserStmt
parserBlock True = Block <$> brackets (many parserStmt)

parserIf :: Parser (Stmt String)
parserIf
  = f <$> rword "if"       <*>
          parserExpr       <*>
          rword "then"     <*>
          parserBlock True <*>
          parserElse
  where
    f _ c _ t e = If c t e
    parserElse = option (Block []) (rword "else" *> parserBlock True)

parserPrint :: Parser (Stmt String)
parserPrint = Print <$> (rword "print" *> parserExpr <* parserSemi)

parserRead :: Parser (Stmt String)
parserRead = SRead <$> (rword "read" *> parserVar <* parserSemi)

parserWhile :: Parser (Stmt String)
parserWhile
  = f <$> rword "while" <*>
          parserExpr    <*>
          rword "do"    <*>
          parserBlock True
  where
    f _ e _ b = While e b

parserStmt :: Parser (Stmt String)
parserStmt
  = choice [
             try parserSkip
           , try parserAssignment
           , try parserVarDef
           , try parserIf
           , try parserPrint
           , try parserRead
           , parserWhile
           ]

parserProgram :: Parser (Block String)
parserProgram = parserBlock False


parseProgram :: String -> Either String (Program String)
parseProgram s
  = case parse parserProgram "" s of
      Left err -> Left $ errorBundlePretty err
      Right t  -> Right t
