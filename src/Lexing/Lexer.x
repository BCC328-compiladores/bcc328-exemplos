-- Initial Haskell code block
{
module Lexing.Lexer (lexer, testLexer) where
}

-- definition of the wrapper

%wrapper "basic"

-- macros for the lexer definition.
-- first character set macros

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

-- second RE macros

@identifier = $alpha[$alpha $digit]* -- identifiers
@number     = $digit+



-- token declarations.

tokens :-
  $white+                 ; -- removing whitespace
  "//".*                  ; -- removing line comments
  @number                 { \ s -> TNumber (read s) }
  @identifier             { \ s -> TVar s }
  \+                      { \ _ -> TAdd }
  \*                      { \ _ -> TMul }
  \(                      { \ _ -> TLParen }
  \)                      { \ _ -> TRParen }

-- more Haskell code

{
data Token
  = TNumber Int
  | TVar String
  | TAdd
  | TMul
  | TLParen
  | TRParen
  deriving (Eq, Show)

lexer :: String -> [Token]
lexer = alexScanTokens

testLexer :: IO ()
testLexer
  = do
      s <- getLine
      let tokens = lexer s
      mapM_ print tokens
}
