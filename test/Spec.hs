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

tokenizeTests = TestLabel "tokenize" $ TestList $ map TestCase [
     do got <- expectSuccessL $ tokenize "(+ 1 (- 10 5))"
        let expected = ["(", "+", "1", "(", "-", "10", "5", ")", ")"]
        assertEqual "" got expected
    ]

parseTests = TestLabel "parse" $ TestList $ map TestCase [
     do got <- expectSuccessL $ parse ["(", "+", "1", "2", ")"]
        let expected = [ASTFunctionCall [ASTSymbol "+", ASTInteger 1, ASTInteger 2]]
        assertEqual "" got expected

   , do got <- expectErrorL $ parse ["(", "+", "1", "2"]
        let expected = "unbalanced function call"
        assertEqual "" got expected
    ]

evaluateTests = TestLabel "evaluate" $ TestList $ map TestCase [
     do let env = M.fromList [("+", builtinAdd2)] :: Env
        (gotEnv, gotAST) <- expectSuccessL $ evaluate env (ASTFunctionCall [ASTSymbol "+", ASTInteger 1, ASTInteger 2])
        let expectedAST = ASTInteger 3
        assertEqual "" gotAST expectedAST
        assertEqual "" (M.keys gotEnv) (M.keys env)
    ]

e2eTests = TestLabel "e2e" $ TestList $ map TestCase [
     do let env = M.fromList [("-", builtinSubtract2)] :: Env
        let script = "(let sub2 (\\[a b] (- a b)))"
        gotEnv <- expectSuccessL $ runInlineScript env script
        let expectedEnvKeys = ["-", "sub2"]
        assertEqual "" (M.keys gotEnv) expectedEnvKeys
    ]

tests = [
      tokenizeTests
    , parseTests
    , evaluateTests
    , e2eTests
    ]

main :: IO ()
main = void $ runTestTT $ test tests
