{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Builtins where

import qualified Data.Map as M
import Control.Monad.Except ( when, MonadError(throwError) )
import Types

builtinEnv :: Env
builtinEnv = M.fromList [
    ("+", builtinAdd2),
    ("-", builtinSubtract2),
    ("*", builtinMultiply2),
    ("/", builtinDivide2),
    ("head", builtinHead),
    ("tail", builtinTail),
    ("prepend", builtinPrepend)
    ]

builtinAdd2 :: AST
builtinAdd2 =
    let outer _ ast1 = do
            (ASTInteger a) <- assertIsASTInteger ast1
            let inner _ ast2 = do
                    (ASTInteger b) <- assertIsASTInteger ast2
                    return $ ASTInteger $ a + b
            return $ ASTFunction $ inner
     in ASTFunction outer

builtinSubtract2 :: AST
builtinSubtract2 =
    let outer _ ast1 = do
            (ASTInteger a) <- assertIsASTInteger ast1
            let inner _ ast2 = do
                    (ASTInteger b) <- assertIsASTInteger ast2
                    return $ ASTInteger $ a - b
            return $ ASTFunction $ inner
     in ASTFunction outer

builtinMultiply2 :: AST
builtinMultiply2 =
    let outer _ ast1 = do
            (ASTInteger a) <- assertIsASTInteger ast1
            let inner _ ast2 = do
                    (ASTInteger b) <- assertIsASTInteger ast2
                    return $ ASTInteger $ a * b
            return $ ASTFunction $ inner
     in ASTFunction outer

builtinDivide2 :: AST
builtinDivide2 =
    let outer _ ast1 = do
            (ASTInteger a) <- assertIsASTInteger ast1
            let inner _ ast2 = do
                    (ASTInteger b) <- assertIsASTInteger ast2
                    when (b == 0) $ throwError $ LException $ "division by zero"
                    return $ ASTInteger $ a `div` b
            return $ ASTFunction $ inner
     in ASTFunction outer

builtinHead :: AST
builtinHead =
    let outer _ ast = do
            (ASTVector vec) <- assertIsASTVector ast
            when (length vec == 0) $ throwError $ LException $ "head of empty vector"
            return $ head vec
     in ASTFunction outer

builtinTail :: AST
builtinTail =
    let outer _ ast = do
            (ASTVector vec) <- assertIsASTVector ast
            when (length vec == 0) $ throwError $ LException $ "tail of empty vector"
            return $ ASTVector $ tail vec
     in ASTFunction outer

builtinPrepend :: AST
builtinPrepend =
    let outer _ ast1 = do
            let inner _ ast2 = do
                    (ASTVector vec) <- assertIsASTVector ast2
                    return $ ASTVector $ ast1 : vec
            return $ ASTFunction $ inner
     in ASTFunction outer