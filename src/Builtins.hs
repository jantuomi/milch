{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Builtins where

import qualified Data.Map as M
import Control.Monad.Except
import Utils

builtinEnv :: Env
builtinEnv = M.fromList [
    ("+", builtinAdd2),
    ("-", builtinSubtract2),
    ("*", builtinMultiply2),
    ("/", builtinDivide2),
    ("head", builtinHead),
    ("tail", builtinTail),
    ("prepend", builtinPrepend),
    ("print!", builtinPrint),
    ("string-concat", builtinStringConcat2)
    ]

-- maybe make a builtinBinaryFunction?
-- builtinBinaryFunction :: AST -> AST -> (AST -> AST -> AST) -> AST
-- builtinAdd2 = builtinBinaryFunction (ASTInteger a) (ASTInteger b) (\(ASTInteger a) (ASTInteger b) -> ASTInteger $ a + b)
builtinAdd2 :: AST
builtinAdd2 =
    let outer :: LFunction
        outer _ (ASTInteger a) = do
            let inner :: LFunction
                inner _ (ASTInteger b) =
                    return $ ASTInteger $ a + b
                inner _ other = throwL $ "invalid argument to integer add: " ++ show other
            return $ ASTFunction $ inner
        outer _ (ASTDouble a) = do
            let inner :: LFunction
                inner _ (ASTDouble b) =
                    return $ ASTDouble $ a + b
                inner _ other = throwL $ "invalid argument to double add: " ++ show other
            return $ ASTFunction $ inner
        outer _ other = throwL $ "non-numeric argument to add: " ++ show other

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
                    when (b == 0) $ throwL $ "division by zero"
                    return $ ASTInteger $ a `div` b
            return $ ASTFunction $ inner
     in ASTFunction outer

builtinHead :: AST
builtinHead =
    let outer _ ast = do
            (ASTVector vec) <- assertIsASTVector ast
            when (length vec == 0) $ throwL $ "head of empty vector"
            return $ head vec
     in ASTFunction outer

builtinTail :: AST
builtinTail =
    let outer _ ast = do
            (ASTVector vec) <- assertIsASTVector ast
            when (length vec == 0) $ throwL $ "tail of empty vector"
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

builtinPrint :: AST
builtinPrint =
    let outer _ ast =
         do (ASTString str) <- assertIsASTString ast
            liftIO $ putStr $ str
            return ASTUnit
     in ASTFunction outer

builtinStringConcat2 :: AST
builtinStringConcat2 =
    let outer _ ast1 = do
            (ASTString str1) <- assertIsASTString ast1
            let inner _ ast2 = do
                    (ASTString str2) <- assertIsASTString ast2
                    return $ ASTString $ str1 ++ str2
            return $ ASTFunction $ inner
     in ASTFunction outer
