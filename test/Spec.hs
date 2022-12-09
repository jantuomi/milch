{-# OPTIONS_GHC -Wno-missing-signatures #-}
import Test.HUnit
import Control.Monad.Except
import qualified Data.Map as M
import qualified Data.List as L
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
        assertEqual "" expected got,

     do got <- expectErrorL M.empty $ parse (map makeNonsenseToken ["(", "+", "1", "2"])
        let expected = "error: unbalanced function call"
        assertEqual "" expected got,

     do (got, _) <- expectSuccessL M.empty $ parse (map makeNonsenseToken ["\"1000\n2000\n3000\""])
        let expected = [astString "1000\n2000\n3000"]
        assertEqual "" expected got
    ]

evaluateTests = testGroup "evaluate" [
     do let env = makeEnv [builtinAdd2]
        (gotAST, LState { stateEnv = gotEnv }) <- expectSuccessL env $
            evaluate (astFunctionCall [astSymbol "+", astInteger 1, astInteger 2])

        let expectedAST = astInteger 3
        assertEqual "" expectedAST gotAST
        assertEqual "" (M.keys env) (M.keys gotEnv)
    ]

e2eTests = testGroup "e2e" [
     do let env = makeEnv [builtinSubtract2]
        let script1 = "(let sub2 (\\[a b] (- a b)))\n(sub2 3 2)"
        (gotASTs, LState { stateEnv = gotEnv }) <- expectSuccessL env $
            runInlineScript "<test>" script1

        let expectedEnvKeys = ["-", "sub2"]
        assertEqual "" expectedEnvKeys (M.keys gotEnv)
        assertEqual "" (astInteger 1) (last gotASTs),

     do let env = M.empty :: Env
        let script1 = "(record A foo bar)"
        (gotASTs, LState { stateEnv = gotEnv }) <- expectSuccessL env $
            runInlineScript "<test>" script1

        let expectedEnvKeys = L.sort ["A/create", "A/get-foo", "A/set-foo", "A/get-bar", "A/set-bar"]
        assertEqual "" expectedEnvKeys (L.sort $ M.keys gotEnv)
        assertEqual "" astUnit (last gotASTs),

     do let env = M.empty :: Env
        let script1 = "(record A foo)\n(let a (A/create 123))\n(A/get-foo a)"
        (gotASTs, _) <- expectSuccessL env $ runInlineScript "<test>" script1

        let expectedLastAST = ast $ ASTInteger 123
        assertEqual "" expectedLastAST (last gotASTs),

     do let env = makeEnv [builtinSortByFirst]
        let script1 = "(sort-by-first [[2 1] [3 2] [1 3]])"
        (gotASTs, _) <- expectSuccessL env $ runInlineScript "<test>" script1

        let expectedLastAST = astVector $
                [astVector [astInteger 1, astInteger 3],
                 astVector [astInteger 2, astInteger 1],
                 astVector [astInteger 3, astInteger 2]]
        assertEqual "" expectedLastAST (last gotASTs),

     do let env = makeEnv [builtinAdd2, builtinMultiply2]
        let script1 = "(let f (\\[x] (let y (+ x 1)) (let z (+ 3 y)) (* 2 z)))\n(f 1)"
        (gotASTs, _) <- expectSuccessL env $ runInlineScript "<test>" script1

        let expectedLastAST = astInteger 10
        assertEqual "" expectedLastAST (last gotASTs),

     do let env = makeEnv [builtinAdd2, builtinSubtract2, ("_", astHole)]
        let script1 = "(let memo fibo (\\[n]\
                      \  (match n\
                      \    0  0\
                      \    1  1\
                      \    _  (+ (fibo (- n 1)) (fibo (- n 2))))))\
                      \ \
                      \(fibo 50)"
        (gotASTs, _) <- expectSuccessL env $ runInlineScript "<test>" script1

        let expectedLastAST = astInteger 12586269025
        assertEqual "" expectedLastAST (last gotASTs),

     do let env = builtinEnv
        let script1 = "(let a :thing)\
                      \(let b :thing)\
                      \(eq? a b)"
        (gotASTs, _) <- expectSuccessL env $ runInlineScript "<test>" script1

        let expectedLastAST = astBoolean True
        assertEqual "" expectedLastAST (last gotASTs),

     do let env = builtinEnv
        script1 <- readFile "test/scripts/record1.milch"
        (gotASTs, _) <- expectSuccessL env $ runInlineScript "<test>" script1

        let expectedLastAST = astInteger 369
        assertEqual "" expectedLastAST (last gotASTs),

     do let env = builtinEnv
        script1 <- readFile "test/scripts/do1.milch"
        (gotASTs, _) <- expectSuccessL env $ runInlineScript "<test>" script1

        let expectedLastAST = astString "foo"
        assertEqual "" expectedLastAST (last gotASTs),

     do let env = builtinEnv
        script1 <- readFile "test/scripts/try1.milch"
        (gotASTs, _) <- expectSuccessL env $ runInlineScript "<test>" script1

        let expectedLastAST = astRecord "Result/Ex" $
                M.fromList [("value", astString "failed to read file: does-not-exist.txt")]
        assertEqual "" expectedLastAST (last gotASTs)
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
