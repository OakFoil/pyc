module Main (main) where

import Compiler.Parsing.Types
import Compiler.Parsing.Parser
import System.FilePath
import Data.Maybe
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let argFileName = fromMaybe "main" $ listToMaybe args
  let resolvedFileName = addExtensionIfMissing argFileName
  input <- readFile resolvedFileName
  stmts <- either fail return $ runMyParser file resolvedFileName input
  print stmts

addExtensionIfMissing :: FilePath -> FilePath
addExtensionIfMissing fileName
  | null $ takeExtension fileName = fileName <.> "py"
  | otherwise = fileName
