{-# OPTIONS_GHC -Wno-missing-signatures #-}
import Test.HUnit
import Control.Monad.Except
import Tokenizer ( tokenize )
import TestUtils

test1 = (runLContext $ tokenize "+ 1 2")
    >>= assertEqual "tokenize \"+ 1 2\"" ["+", "1", "2"]

tests = TestList $ map TestCase [
    test1
    ]

main :: IO ()
main = void $ runTestTT tests
