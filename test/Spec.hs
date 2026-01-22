import Parser qualified
import Test.HUnit

main :: IO ()
main = runTestTT (test Parser.tests) >>= print
