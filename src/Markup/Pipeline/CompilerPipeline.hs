module Markup.Pipeline.CompilerPipeline ( createHandles
                                        , startPipeline
                                        , optionsParser
                                        , hClose
                                        ) where

import Control.Monad (when, unless)
import Data.List (partition)
import Markup.Printer.Html (Head, render, title_)
import Markup.Language.Frontend
import Markup.Printer.Backend
import Markup.Pipeline.OptionsParser
import System.Directory ( createDirectory
                        , removeDirectoryRecursive
                        , listDirectory
                        , doesDirectoryExist
                        , doesFileExist
                        , copyFile
                        )
import System.FilePath ( takeExtension
                       , takeBaseName
                       , (<.>)
                       , (</>)
                       , takeFileName
                       )
import System.Exit (exitFailure, exitSuccess)
import System.IO

---------------------------------------------------
-- Creating handles                              --
---------------------------------------------------

createHandles :: Input -> Output -> IO (String, Handle, Handle)
createHandles inp out
  = do
      (title, inpHandle) <- createInputHandle inp
      outHandle <- createOutputHandle out
      return (title, inpHandle, outHandle)

createInputHandle :: Input -> IO (String, Handle)
createInputHandle Stdin = return ("", stdin)
createInputHandle (FileInput path)
  = (,) path <$> openFile path ReadMode

confirmOverwrite :: FilePath -> IO Bool
confirmOverwrite path
  = do
      putStrLn $ "\n" ++ path ++ "\nexists. Confirm overwrite?"
      answer <- getLine
      case answer of
        "y" -> return True
        "n" -> return False
        _   -> do
                 putStrLn "Invalid answer! Please type y or n."
                 confirmOverwrite path

createOutputHandle :: Output -> IO Handle
createOutputHandle Stdout = return stdout
createOutputHandle (FileOutput path)
  = do
      exists <- doesFileExist path
      canWrite <- if exists then confirmOverwrite path
                  else return True
      if canWrite then openFile path WriteMode
        else exitSuccess

---------------------------------------------------
-- compilation pipeline                          --
---------------------------------------------------

startPipeline :: IO ()
startPipeline
  = do
      options <- optionsParser
      case options of
        Single inp out -> filePipeline False inp out
        Directory inpDir outDir -> directoryPipeline inpDir outDir

filePipeline :: Bool -> Input -> Output -> IO ()
filePipeline dirMode inpFile outFile 
  = do
      progressMessage dirMode inpFile
      (title,inpHandle,outHandle) <- createHandles inpFile outFile
      content <- hGetContents inpHandle
      res <- pipeline (title_ title) content
      hPutStrLn outHandle res
      hClose inpHandle
      hClose outHandle

progressMessage :: Bool -> Input -> IO ()
progressMessage False _ = return ()
progressMessage True Stdin = return ()
progressMessage True (FileInput file)
  = putStrLn $ "Processing file " ++ file

pipeline :: Head -> String -> IO String
pipeline title content
  = case frontEnd content of
      Left err -> putStrLn err >> exitFailure
      Right ast -> return $ render $ backEnd title ast

---------------------------------------------------
-- Directory processing functions                --
---------------------------------------------------

data DirContents
  = DirContents {
       filesToCompile :: [(FilePath, FilePath)]
     , filesToCopy :: [FilePath]
    }

directoryContents :: FilePath -> IO DirContents
directoryContents inputDir
  = do
       files <- map (inputDir </>) <$> listDirectory inputDir
       let
         select = (== ".md") . takeExtension
         (mdFiles, otherFiles) = partition select files
         htmlFiles = map (\ f -> takeBaseName f <.> "html") mdFiles
       return $ DirContents (zip mdFiles htmlFiles) otherFiles

directoryPipeline :: FilePath -> FilePath -> IO ()
directoryPipeline inputDir outputDir
  = do
      DirContents files otherFiles <- directoryContents inputDir
      let entries = map (\ (i,o) -> (FileInput i, FileOutput o)) files
      shouldContinue <- createOutputDirectory outputDir
      unless shouldContinue (hPutStrLn stderr "Cancelled." *> exitFailure)
      mapM_ (uncurry (filePipeline True)) entries
      putStrLn "Done."


createOutputDirectory :: FilePath -> IO Bool
createOutputDirectory outDir
  = do
      dirExists <- doesDirectoryExist outDir
      shouldCreate <-
        if dirExists then do
          override <- confirmOverwrite outDir
          when override (removeDirectoryRecursive outDir)
          return override
        else return True
      when shouldCreate (createDirectory outDir)
      return shouldCreate
