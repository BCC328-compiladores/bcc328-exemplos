module Exp.Backend.Backend (backEnd) where

import System.FilePath
import qualified Data.ByteString as B

import Exp.Frontend.Typing.TyExp
import Exp.Backend.LLVM.ExpCodegen
import Exp.Backend.LLVM.Codegen

backEnd :: FilePath -> TyExp -> IO ()
backEnd path e
  = do
       let output = replaceExtension path "ll"
       m <- codeGen (emptyModule "") e
       B.putStr m
       B.writeFile output m
       return ()

