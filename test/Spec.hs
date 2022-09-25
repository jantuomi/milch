{-# OPTIONS_GHC -Wno-missing-signatures #-}
import Test.HUnit
import Control.Monad.Except
import Tokenizer ( tokenize )
import Parser ( parse )
import TestUtils
import Utils

test1 = (runLContext $ tokenize "(+ 1 (- 10 5))")
    >>= assertEqual "tokenize" ["(", "+", "1", "(", "-", "10", "5", ")", ")"]

test2 = (runLContext $ parse ["(", "+", "1", "2", ")"])
    >>= assertEqual "parse" [ASTFunctionCall [ASTSymbol "+", ASTInteger 1, ASTInteger 2]]

tests = TestList $ map TestCase [
    test1,
    test2
    ]

main :: IO ()
main = void $ runTestTT tests
