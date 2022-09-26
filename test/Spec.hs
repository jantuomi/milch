{-# OPTIONS_GHC -Wno-missing-signatures #-}
import Test.HUnit
import Control.Monad.Except
import qualified Data.Map as M
import Tokenizer ( tokenize )
import Parser ( parse )
import Evaluator ( evaluate )
import Builtins
import TestUtils
import Utils

tests = [
    "tokenize1" ~:
         do got <- runL $ tokenize "(+ 1 (- 10 5))"
            let expected = ["(", "+", "1", "(", "-", "10", "5", ")", ")"]
            assertEqual "" got expected,
    "parse1" ~:
         do got <- runL $ parse ["(", "+", "1", "2", ")"]
            let expected = [ASTFunctionCall [ASTSymbol "+", ASTInteger 1, ASTInteger 2]]
            assertEqual "" got expected,
    "evaluate1" ~:
         do let env = M.fromList [("+", builtinAdd2)] :: Env
            (gotEnv, gotAST) <- runL $ evaluate env (ASTFunctionCall [ASTSymbol "+", ASTInteger 1, ASTInteger 2])
            let expectedAST = ASTInteger 3
            assertEqual "" gotAST expectedAST
            assertEqual "" (M.keys gotEnv) (M.keys env)
    ]

main :: IO ()
main = void $ runTestTT $ test tests
