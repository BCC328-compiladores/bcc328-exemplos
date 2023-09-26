module Imp.Backend.Stack.StatementCodegen (compileProgram) where

import Imp.Frontend.Syntax.Exp
import Imp.Frontend.Syntax.Statement
import Imp.Backend.Stack.ExprCodegen
import Stack.Frontend.Syntax.Instr

compileProgram :: Program String -> IO Code
compileProgram blk
  =  (flip (++) [HALT]) <$> runCompileM (compileBlock blk)

compileStatement :: Stmt String -> CompileM Code
compileStatement Skip = return []
compileStatement (Def _ v einit)
  = do
      addr <- lookupVar v
      c1 <- compileInit einit
      return $ PUSHI addr : c1 ++ [STORE]
compileStatement (v := e)
  = do
      addr <- lookupVar v
      c1 <- compileExpr e
      return $ PUSHI addr : c1 ++ [STORE]
compileStatement (Print e)
  = do
      c <- compileExpr e
      return (c ++ [OUT])
compileStatement (SRead v)
  = do
      addr <- lookupVar v
      return $ [PUSHI addr, IN, STORE]
compileStatement (If e bthen belse)
  = do
      ce <- compileExpr e
      cthen <- compileBlock bthen
      celse <- compileBlock belse
      let thensize = length cthen
          elsesize = length celse
      return $ concat [ ce, [JZ $ thensize + 1]
                      , cthen, [JMP $ elsesize + 1]
                      , celse]
compileStatement (While e blk)
  = do
      ce <- compileExpr e
      cblock <- compileBlock blk
      let blksize = length cblock
          expsize = length ce
          back = - (blksize + expsize + 1)
      return $ concat [ ce, [JZ (blksize + 2)], cblock
                      , [JMP back]
                      ]

compileInit :: Maybe (Exp String) -> CompileM Code
compileInit Nothing = return [PUSHI 0]
compileInit (Just e) = compileExpr e

compileBlock :: Block String -> CompileM Code
compileBlock (Block blk)
  = concat <$> mapM compileStatement blk
