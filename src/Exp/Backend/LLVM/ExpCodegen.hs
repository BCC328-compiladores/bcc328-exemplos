{-# OPTIONS_GHC -Wno-unused-do-bind#-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE OverloadedStrings     #-}

module Exp.Backend.LLVM.ExpCodegen (codeGen) where

import qualified LLVM.AST.IntegerPredicate     as IP
import           LLVM.AST                       ( Operand )
import qualified LLVM.AST                      as AST
import qualified LLVM.AST.Type                 as AST
import           LLVM.AST.Name

import qualified LLVM.IRBuilder.Module         as L
import qualified LLVM.IRBuilder.Monad          as L
import qualified LLVM.IRBuilder.Instruction    as L
import qualified LLVM.IRBuilder.Constant       as L

import qualified Data.Map                      as M
import           Control.Monad.State

import           Exp.Frontend.Typing.TyExp

import           Data.Text                      ( Text, unpack )


-- When using the IRBuilder, both functions and variables have the type Operand

data Env = Env { operands :: M.Map Text Operand
               , strings :: M.Map Text Operand
               }
  deriving (Eq, Show)


type LLVM = L.ModuleBuilderT (State Env)
type Codegen = L.IRBuilderT LLVM

-- register a function or variable in the code generation environment

registerOperand :: MonadState Env m => Text -> Operand -> m ()
registerOperand name op =
  modify $ \env -> env { operands = M.insert name op (operands env) }

-- type definitions

charStar :: AST.Type
charStar = AST.ptr AST.i8

int :: AST.Type
int = AST.i32

-- code generation for expressions

genBinOp :: (Operand -> Operand -> Codegen Operand) -> TyExp -> TyExp -> Codegen Operand
genBinOp f e1 e2 
  = do
      c1 <- codeGenExpr e1
      c2 <- codeGenExpr e2
      f c1 c2

codeGenExpr :: TyExp -> Codegen Operand
codeGenExpr (_ , TInt n)
  = pure $ L.int32 (fromIntegral n)
codeGenExpr (_, TBool b)
  = pure $ L.bit (if b then 1 else 0)
codeGenExpr (_, TAdd e1 e2)
  = genBinOp L.add e1 e2
codeGenExpr (_, TSub e1 e2)
  = genBinOp L.sub e1 e2
codeGenExpr (_, TIsZero e1)
  = genBinOp (L.icmp IP.EQ) e1 (Int, TInt 0)
codeGenExpr (_, TAnd e1 e2)
  = genBinOp L.and e1 e2
codeGenExpr (_, TNot e1)
  = genBinOp L.xor e1 (Bool, TBool True)
codeGenExpr (_, TIf e1 e2 e3)
  = mdo
      cond <- codeGenExpr e1
      L.condBr cond thenBlock elseBlock
      thenBlock <- L.block `L.named` "then"
      thenRes <- codeGenExpr e2
      elseBlock <- L.block `L.named` "else"
      elseRes <- codeGenExpr e3
      L.phi [(thenRes, thenBlock), (elseRes, elseBlock)]

-- strings need to be defined as globals

addString :: Text -> Codegen Operand
addString s
  = do
      strs <- gets strings
      case M.lookup s strs of
        Nothing -> do
          let nm = mkName (show (M.size strs) <> ".str")
          op <- L.globalStringPtr (unpack s) nm
          modify $ \env -> env { strings = M.insert s (AST.ConstantOperand op) strs }
          pure (AST.ConstantOperand op)
        Just op -> pure op


codeGenMain :: Operand -> TyExp -> LLVM ()
codeGenMain f e
  = mdo
      registerOperand "main" fmain
      fmain <- L.function "main" [] int genBody
      return ()
    where
      -- hack to print the result at the end
      genBody :: [Operand] -> Codegen ()
      genBody _
         = do
             op <- addString "%d"
             body <- codeGenExpr e
             zero <- codeGenExpr (Int, TInt 0)
             L.call f [(op, []), (body, [])]
             L.ret zero
             return ()
  
codeGen :: TyExp -> AST.Module
codeGen e
  = flip evalState (Env { operands = M.empty , strings = M.empty })
    $ L.buildModuleT "exp"
    $ do
        printf <- L.externVarArgs (mkName "printf") [charStar] int
        registerOperand "printf" printf
        codeGenMain printf e
