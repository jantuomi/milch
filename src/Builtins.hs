module Builtins where

import qualified Data.Map as M
import Control.Monad.Except
import Types

builtinEnv :: Env
builtinEnv = M.fromList [
    ("+", builtinAdd2),
    ("-", builtinSubtract2),
    ("head", builtinHead),
    ("tail", builtinTail),
    ("prepend", builtinPrepend)
    ]

builtinAdd2 :: AST
builtinAdd2 =
    let outer ast1 = do
            (ASTInteger a) <- assertIsASTInteger ast1
            let inner ast2 = do
                    (ASTInteger b) <- assertIsASTInteger ast2
                    return $ ASTInteger $ a + b
            return $ ASTFunction $ inner
     in ASTFunction outer

builtinSubtract2 :: AST
builtinSubtract2 =
    let outer ast1 = do
            (ASTInteger a) <- assertIsASTInteger ast1
            let inner ast2 = do
                    (ASTInteger b) <- assertIsASTInteger ast2
                    return $ ASTInteger $ a - b
            return $ ASTFunction $ inner
     in ASTFunction outer

builtinHead :: AST
builtinHead =
    let outer ast = do
            (ASTVector vec) <- assertIsASTVector ast
            when (length vec == 0) $ throwError $ LException $ "head of empty vector"
            return $ head vec
     in ASTFunction outer

builtinTail :: AST
builtinTail =
    let outer ast = do
            (ASTVector vec) <- assertIsASTVector ast
            when (length vec == 0) $ throwError $ LException $ "tail of empty vector"
            return $ ASTVector $ tail vec
     in ASTFunction outer

builtinPrepend :: AST
builtinPrepend =
    let outer ast1 = do
            let inner ast2 = do
                    (ASTVector vec) <- assertIsASTVector ast2
                    return $ ASTVector $ ast1 : vec
            return $ ASTFunction $ inner
     in ASTFunction outer