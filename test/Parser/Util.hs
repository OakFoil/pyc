module Parser.Util (testIfParsesWithoutError, testIfParsesWithError, testIfParsesCorrectly) where

import Compiler.Parsing.Types
import Data.Either.Extra
import Test.HUnit
import Text.Megaparsec

testIfParsesWithoutError :: Parser a -> Input -> Assertion
testIfParsesWithoutError parser string =
  assertBool ("Test Case: " ++ string ++ "\nGot: " ++ fromLeft' parseResult) $
    isRight parseResult
  where
    parseResult = runMyParser (parser <* eof) "main.py" string

testIfParsesWithError :: (Show a) => Parser a -> Input -> Assertion
testIfParsesWithError parser string =
  assertBool ("Test Case: " ++ string ++ "\nShould Error Instead Got: " ++ show (fromRight' parseResult)) $
    isLeft parseResult
  where
    parseResult = runMyParser (parser <* eof) "main.py" string

testIfParsesCorrectly :: (Eq a, Show a) => Parser a -> (Input, a) -> Assertion
testIfParsesCorrectly parser (string, correctParseResult) = do
  testIfParsesWithoutError parser string
  assertEqual ("Test Case: " ++ string) (fromRight' actualParseResult) correctParseResult
  where
    actualParseResult = runMyParser (parser <* eof) "main.py" string
