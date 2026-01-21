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
      testParseCase
      [ "from package import variable",
        "a = a",
        "1",
        "lambda a, b: a + b",
        "f(a, b, c)"
      ]

testParseCase :: Input -> Test
testParseCase string = TestCase $ assertBool ("Got: " ++ fromLeft' parseResult) $ isRight parseResult
  where
    parseResult = runMyParser file "main.py" string
