import Compiler.Parsing.Parser
import Compiler.Parsing.Types
import Data.Either
import Data.Either.Extra (fromLeft')
import Test.HUnit

main :: IO ()
main = runTestTT parsingTests >>= print

parsingTests :: Test
parsingTests =
  TestList $
    map
      testParsing
      [ "from package import variable",
        "a = a",
        "1",
        "lambda a, b: a + b",
        "f(a, b, c)"
      ]

testParsing :: Input -> Test
testParsing string = TestCase $ assertBool ("Got: " ++ fromLeft' parseResult) $ isRight parseResult
  where
    parseResult = runMyParser file "main.py" string
