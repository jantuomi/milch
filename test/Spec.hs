{-# OPTIONS_GHC -Wno-missing-signatures #-}
import Test.HUnit
import Control.Monad.Except
import qualified Data.Map as M
import Builtins
import Tokenizer ( tokenize )
import Parser ( parse )
import Evaluator ( evaluate )
import Lib ( runInlineScript )
import Utils
import TestUtils

tokenizeTests = testGroup "tokenize" [
     do got <- expectSuccessL $ tokenize "(+ 1 (- 10 5))"
        let expected = ["(", "+", "1", "(", "-", "10", "5", ")", ")"]
        assertEqual "" got expected
    ]

parseTests = testGroup "parse" [
     do got <- expectSuccessL $ parse ["(", "+", "1", "2", ")"]
        let expected = [ASTFunctionCall [ASTSymbol "+", ASTInteger 1, ASTInteger 2]]
        assertEqual "" got expected

   , do got <- expectErrorL $ parse ["(", "+", "1", "2"]
        let expected = "unbalanced function call"
        assertEqual "" got expected
    ]

evaluateTests = testGroup "evaluate" [
     do let env = M.fromList [("+", builtinAdd2)] :: Env
        (gotEnv, gotAST) <- expectSuccessL $ evaluate env (ASTFunctionCall [ASTSymbol "+", ASTInteger 1, ASTInteger 2])
        let expectedAST = ASTInteger 3
        assertEqual "" gotAST expectedAST
        assertEqual "" (M.keys gotEnv) (M.keys env)
    ]

e2eTests = testGroup "e2e" [
     do let env = M.fromList [("-", builtinSubtract2)] :: Env
        let script1 = "(let sub2 (\\[a b] (- a b)))\n(sub2 3 2)"
        (gotEnv, gotASTs) <- expectSuccessL $ runInlineScript env script1
        let expectedEnvKeys = ["-", "sub2"]
        assertEqual "" (M.keys gotEnv) expectedEnvKeys
        assertEqual "" (last gotASTs) (ASTInteger 1)
    ]

testGroup label xs = TestLabel label $ TestList $ map TestCase xs

tests = [
      tokenizeTests
    , parseTests
    , evaluateTests
    , e2eTests
    ]

main :: IO ()
main = void $ runTestTT $ test tests
