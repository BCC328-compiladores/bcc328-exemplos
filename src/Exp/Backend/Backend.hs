module Exp.Backend.Backend (backEnd) where

import           LLVM.AST

import qualified LLVM.Module                   as LLVM
import           LLVM.Context
import           LLVM.Analysis

import           System.FilePath
import           System.Process

import           Exp.Frontend.Typing.TyExp
import           Exp.Backend.LLVM.ExpCodegen

-- Generate an executable at the given filepath from an llvm module

compile :: Module -> FilePath -> IO ()
compile llvmModule path =
  do
      let
        dir = takeDirectory path
        file = takeFileName path
        base = dropExtension file
        llvm = dir </> base <.> "ll"
      withContext $ \ctx -> LLVM.withModuleFromAST
        ctx
        llvmModule
        (\modl -> verify modl >> LLVM.writeBitcodeToFile (LLVM.File llvm) modl)
      -- link the runtime with the assembly
      callProcess "clang" ["-Wno-override-module", llvm, "-o", dir </> base]

backEnd :: FilePath -> TyExp -> IO ()
backEnd path e
  = do
      let llvmMod = codeGen e
      compile llvmMod path
