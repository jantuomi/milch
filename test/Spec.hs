{-# OPTIONS_GHC -Wno-missing-signatures #-}
import Test.HUnit
import Control.Monad.Except
import qualified Data.Map as M
import Builtins
import Tokenizer ( tokenize )
import Parser ( parse )
import Interpreter ( evaluate, runInlineScript )
import Utils
import TestUtils

tokenizeTests = testGroup "tokenize" [
     do (got, _) <- expectSuccessL M.empty $ tokenize "<test>" "(+ 1 (- 10 5))"
        let expected = ["(", "+", "1", "(", "-", "10", "5", ")", ")"]
        assertEqual "" (map tokenContent got) expected
    ]

parseTests = testGroup "parse" [
     do (got, _) <- expectSuccessL M.empty $ parse (map makeNonsenseToken ["(", "+", "1", "2", ")"])
        let expected = [astFunctionCall
                            [ astSymbol "+", astInteger 1, astInteger 2 ]]
        assertEqual "" got expected

   , do got <- expectErrorL M.empty $ parse (map makeNonsenseToken ["(", "+", "1", "2"])
        let expected = "unbalanced function call"
        assertEqual "" got expected
    ]

evaluateTests = testGroup "evaluate" [
     do let env = M.fromList [builtinAdd2] :: Env
        (gotAST, LState { stateEnv = gotEnv }) <- expectSuccessL env $
            evaluate (astFunctionCall [astSymbol "+", astInteger 1, astInteger 2])

        let expectedAST = astInteger 3
        assertEqual "" gotAST expectedAST
        assertEqual "" (M.keys gotEnv) (M.keys env)
    ]

e2eTests = testGroup "e2e" [
     do let env = M.fromList [builtinSubtract2] :: Env
        let script1 = "(let! sub2 (\\[a b] (- a b)))\n(sub2 3 2)"
        (gotASTs, LState { stateEnv = gotEnv }) <- expectSuccessL env $
            runInlineScript "<test>" script1

        let expectedEnvKeys = ["-", "sub2"]
        assertEqual "" (M.keys gotEnv) expectedEnvKeys
        assertEqual "" (last gotASTs) (astInteger 1)
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
