module Main (main) where

import Compiler.Parsing.Parser
import Compiler.Parsing.Types
import Data.Maybe
import System.Environment
import System.Exit
import System.FilePath
import System.IO

main :: IO ()
main = do
  args <- getArgs
  let argFileName = fromMaybe "main" $ listToMaybe args
  let resolvedFileName = addExtensionIfMissing argFileName
  input <- readFile resolvedFileName
  stmts <- either exitWithErrorMsg return $ runMyParser file resolvedFileName input
  print stmts
  where
    exitWithErrorMsg a = do
      hPutStr stderr a
      exitFailure

addExtensionIfMissing :: FilePath -> FilePath
addExtensionIfMissing fileName
  | null $ takeExtension fileName = fileName <.> "py"
  | otherwise = fileName
