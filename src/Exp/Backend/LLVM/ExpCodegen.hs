{-# OPTIONS_GHC -Wno-unused-do-bind#-}
module Exp.Backend.LLVM.ExpCodegen (codeGen) where

import LLVM.Module
import LLVM.Context

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Operand as L
import qualified Data.ByteString as B

import Exp.Backend.LLVM.Codegen
import Exp.Frontend.Typing.TyExp

expGen :: TyExp -> LLVM ()
expGen ex = do
  external int "printf" [(charStar , AST.Name $ toName "args")]
  define (typeOf $ fst ex) "main" [] blks
  where
    blks = createBlocks $ execCodegen $ do
      entry1 <- addBlock entryBlockName
      _ <- setBlock entry1
      expGen' ex >>= ret


genBinOp :: TyExp -> TyExp ->
            (L.Operand -> L.Operand -> Codegen L.Operand) ->
            Codegen L.Operand
genBinOp e1 e2 f
  = do
      c1 <- expGen' e1
      c2 <- expGen' e2
      f c1 c2

expGen' :: TyExp -> Codegen L.Operand
expGen' (_, TInt n)
  = return $ cons $ C.Int 32 (fromIntegral n)
expGen' (_, TBool b)
  = return $ cons $ C.Int 1  (if b then 1 else 0)
expGen' (_, TAdd e1 e2)
  = genBinOp e1 e2 fadd
expGen' (_, TSub e1 e2)
  = genBinOp e1 e2 fsub
expGen' (_, TAnd e1 e2)
  = genBinOp e1 e2 fand
expGen' (_, TIsZero e1)
  = genBinOp e1 (Int, TInt 0) feq
expGen' (_, TNot e1)
  = genBinOp e1 (Bool, TBool True) fxor
expGen' (t, TIf e1 e2 e3)
  = do
     ifthen <- addBlock "if.then"
     ifelse <- addBlock "if.else"
     ifexit <- addBlock "if.exit"
     -- code generation for condition
     c1 <- expGen' e1
     c0 <- expGen' (Bool, TBool True)
     test <- feq c0 c1
     cbr test ifthen ifelse
     -- code generation for then
     setBlock ifthen
     c2 <- expGen' e2
     br ifexit
     ifthen1 <- getBlock
     -- code generation for else
     setBlock ifelse
     c3 <- expGen' e3
     br ifexit
     ifelse1 <- getBlock
     -- code generation for exit
     setBlock ifexit
     phi (typeOf t) [(c2, ifthen1), (c3, ifelse1)]

typeOf :: Ty -> AST.Type
typeOf Int = int
typeOf Bool = bool


-- top level code generation

codeGen :: AST.Module -> TyExp -> IO B.ByteString
codeGen md e
  = withContext $ \ ctx ->
        withModuleFromAST ctx newast $
         \ m -> moduleLLVMAssembly m
    where
      newast = runLLVM md (expGen e)
