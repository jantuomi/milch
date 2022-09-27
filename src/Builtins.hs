{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Builtins where

import qualified Data.Map as M
import qualified Data.Text as T
import Control.Monad.Except
import Utils

builtinEnv :: Env
builtinEnv = M.fromList [
    builtinAdd2,
    builtinSubtract2,
    builtinMultiply2,
    builtinDivide2,
    builtinHead,
    builtinTail,
    builtinPrepend,
    builtinPrint,
    builtinConcat,
    builtinFmt,
    ("unit", ASTUnit),
    builtinFatal
    ]

argError1 :: Show a => String -> a -> String
argError1 fn arg = "invalid argument to " ++ fn ++ ": " ++ show arg

argError2 :: (Show a1, Show a2) => String -> a1 -> a2 -> String
argError2 fn arg1 arg2 = "invalid arguments to " ++ fn ++ ": " ++ show arg1 ++ ", " ++ show arg2

-- BUILTINS

builtinAdd2 :: (String, AST)
builtinAdd2 = (name, ASTFunction outer) where
    name = "+"
    outer _ ast1@(ASTInteger a) =
        return $ ASTFunction $ inner where
            inner _ (ASTInteger b) =
                return $ ASTInteger $ a + b
            inner _ ast2 = throwL $ argError2 name ast1 ast2
    outer _ ast1@(ASTDouble a) =
        return $ ASTFunction $ inner where
            inner _ (ASTDouble b) =
                return $ ASTDouble $ a + b
            inner _ ast2 = throwL $ argError2 name ast1 ast2
    outer _ ast1 = throwL $ argError1 name ast1

builtinSubtract2 :: (String, AST)
builtinSubtract2 = (name, ASTFunction outer) where
    name = "-"
    outer _ ast1@(ASTInteger a) =
        return $ ASTFunction $ inner where
            inner _ (ASTInteger b) =
                return $ ASTInteger $ a - b
            inner _ ast2 = throwL $ argError2 name ast1 ast2
    outer _ ast1@(ASTDouble a) =
        return $ ASTFunction $ inner where
            inner _ (ASTDouble b) =
                return $ ASTDouble $ a - b
            inner _ ast2 = throwL $ argError2 name ast1 ast2
    outer _ ast1 = throwL $ argError1 name ast1

builtinMultiply2 :: (String, AST)
builtinMultiply2 = (name, ASTFunction outer) where
    name = "*"
    outer _ ast1@(ASTInteger a) =
        return $ ASTFunction $ inner where
            inner _ (ASTInteger b) =
                return $ ASTInteger $ a * b
            inner _ ast2 = throwL $ argError2 name ast1 ast2
    outer _ ast1@(ASTDouble a) =
        return $ ASTFunction $ inner where
            inner _ (ASTDouble b) =
                return $ ASTDouble $ a * b
            inner _ ast2 = throwL $ argError2 name ast1 ast2
    outer _ ast1 = throwL $ argError1 name ast1

builtinDivide2 :: (String, AST)
builtinDivide2 = (name, ASTFunction outer) where
    name = "/"
    outer _ ast1@(ASTInteger a) =
        return $ ASTFunction $ inner where
            inner _ (ASTInteger b) =
             do when (b == 0) $ throwL $ "division by zero"
                return $ ASTInteger $ a `div` b
            inner _ ast2 = throwL $ argError2 name ast1 ast2
    outer _ ast1@(ASTDouble a) =
        return $ ASTFunction $ inner where
            inner _ (ASTDouble b) =
             do when (b == 0) $ throwL $ "division by zero"
                return $ ASTDouble $ a / b
            inner _ ast2 = throwL $ argError2 name ast1 ast2
    outer _ ast1 = throwL $ argError1 name ast1

builtinHead :: (String, AST)
builtinHead = (name, ASTFunction outer) where
    name = "head"
    outer _ (ASTVector vec) =
     do when (length vec == 0) $ throwL $ name ++ " of empty vector"
        return $ head vec
    outer _ ast = throwL $ argError1 name ast

builtinTail :: (String, AST)
builtinTail = (name, ASTFunction outer) where
    name = "tail"
    outer _ (ASTVector vec) =
     do when (length vec == 0) $ throwL $ name ++ " of empty vector"
        return $ ASTVector $ tail vec
    outer _ ast = throwL $ argError1 name ast

builtinPrepend :: (String, AST)
builtinPrepend = (name, ASTFunction outer) where
    name = "prepend"
    outer _ ast1 =
        return $ ASTFunction $ inner where
            inner _ (ASTVector vec) =
                return $ ASTVector $ ast1 : vec
            inner _ ast2 = throwL $ argError2 name ast1 ast2

builtinPrint :: (String, AST)
builtinPrint = (name, ASTFunction outer) where
    name = "print!"
    outer _ (ASTString str) =
     do liftIO $ putStr $ str
        return ASTUnit
    outer _ ast = throwL $ argError1 name ast

builtinConcat :: (String, AST)
builtinConcat = (name, ASTFunction outer) where
    name = "concat"
    outer _ ast1@(ASTString str1) =
        return $ ASTFunction $ inner where
            inner _ (ASTString str2) =
                return $ ASTString $ str1 ++ str2
            inner _ ast2 = throwL $ argError2 name ast1 ast2
    outer _ ast1 = throwL $ argError1 name ast1

builtinFmt :: (String, AST)
builtinFmt = (name, ASTFunction outer) where
    name = "fmt"
    outer _ ast1@(ASTString str) =
        return $ ASTFunction $ inner where
            inner _ (ASTVector replacements) =
                return $ ASTString $ T.unpack $ replaceAll (0 :: Int) replacements (T.pack str)
            inner _ ast2 = throwL $ argError2 name ast1 ast2
    outer _ ast1 = throwL $ argError1 name ast1
    replaceAll _ [] text = text
    replaceAll n (x:xs) text =
        let text' = T.replace (T.pack $ "{" ++ show n ++ "}") (T.pack $ show x) text
         in replaceAll (n + 1) xs text'

builtinFatal :: (String, AST)
builtinFatal = (name, ASTFunction outer) where
    name = "fatal"
    outer _ (ASTString str) =
        throwL $ str
    outer _ ast =
        throwL $ argError1 name ast
