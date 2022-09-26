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

argError1 :: Show a => String -> a -> String
argError1 fn arg = "invalid argument to " ++ fn ++ ": " ++ show arg

argError2 :: (Show a1, Show a2) => String -> a1 -> a2 -> String
argError2 fn arg1 arg2 = "invalid arguments to " ++ fn ++ ": " ++ show arg1 ++ ", " ++ show arg2

-- BUILTINS

builtinAdd2 :: AST
builtinAdd2 =
    let outer _ ast1@(ASTInteger a) = do
            let inner _ (ASTInteger b) =
                    return $ ASTInteger $ a + b
                inner _ ast2 = throwL $ argError2 "+" ast1 ast2
            return $ ASTFunction $ inner
        outer _ ast1@(ASTDouble a) = do
            let inner _ (ASTDouble b) =
                    return $ ASTDouble $ a + b
                inner _ ast2 = throwL $ argError2 "+" ast1 ast2
            return $ ASTFunction $ inner
        outer _ ast1 = throwL $ argError1 "+" ast1
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
