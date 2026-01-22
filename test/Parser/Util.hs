module Parser.Util (testIfParsesWithoutError, testIfParsesCorrectly) where

import Compiler.Parsing.Types
import Data.Either.Extra
import Test.HUnit

testIfParsesWithoutError :: Parser a -> Input -> Assertion
testIfParsesWithoutError parser string =
  assertBool (string ++ "\nGot: " ++ fromLeft' parseResult) $
    isRight parseResult
  where
    parseResult = runMyParser parser "main.py" string

testIfParsesCorrectly :: (Eq a, Show a) => Parser a -> (Input, a) -> Assertion
testIfParsesCorrectly parser (string, correctParseResult) = do
  testIfParsesWithoutError parser string
  assertEqual string (fromRight' actualParseResult) correctParseResult
  where
    actualParseResult = runMyParser parser "main.py" string
