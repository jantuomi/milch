{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Builtins where

import qualified Data.Map as M
import qualified Data.Text as T
import Control.Monad.Except
import Utils

builtinEnv :: Env
builtinEnv = M.fromList [
    -- arithmetic
    builtinAdd2,
    builtinSubtract2,
    builtinMultiply2,
    builtinDivide2,
    -- comparisons
    builtinEq2,
    builtinLt2,
    -- conversions
    builtinFmt,
    builtinFloor,
    builtinToDouble,
    -- vector operations
    builtinHead,
    builtinTail,
    builtinPrepend,
    -- string operations
    builtinSubstr,
    -- vector & string operations
    builtinConcat,
    -- special
    builtinPrint,
    ("unit", makeNonsenseAST ASTUnit),
    ("_", makeNonsenseAST ASTHole),
    ("otherwise", makeNonsenseAST ASTHole),
    builtinFatal
    ]

argError1 :: String -> AST -> String
argError1 fn arg = pos arg ++ ": invalid argument to " ++ fn ++ ": " ++ show arg

argError2 :: (Show a1, Show a2) => String -> a1 -> a2 -> String
argError2 fn arg1 arg2 = "invalid arguments to " ++ fn ++ ": " ++ show arg1 ++ ", " ++ show arg2

argError3 :: (Show a1, Show a2, Show a3) => String -> a1 -> a2 -> a3 -> String
argError3 fn arg1 arg2 arg3 = "invalid arguments to " ++ fn ++ ": " ++ show arg1 ++ ", " ++ show arg2 ++ ", " ++ show arg3

-- BUILTINS

builtinAdd2 :: (String, AST)
builtinAdd2 = (name, makeNonsenseAST $ ASTFunction fn1) where
    name = "+"
    fn1 _ ast1@AST { astNode = ASTInteger a } =
        return $ makeNonsenseAST $ ASTFunction $ fn2 where
            fn2 _ AST { astNode = ASTInteger b } =
                return $ makeNonsenseAST $ ASTInteger $ a + b
            fn2 _ ast2 = throwL $ argError2 name ast1 ast2
    fn1 _ ast1@AST { astNode = ASTDouble a } =
        return $ makeNonsenseAST $ ASTFunction $ fn2 where
            fn2 _ AST { astNode = ASTDouble b } =
                return $ makeNonsenseAST $ ASTDouble $ a + b
            fn2 _ ast2 = throwL $ argError2 name ast1 ast2
    fn1 _ ast1 = throwL $ argError1 name ast1

builtinSubtract2 :: (String, AST)
builtinSubtract2 = (name, makeNonsenseAST $ ASTFunction fn1) where
    name = "-"
    fn1 _ ast1@AST { astNode = ASTInteger a } =
        return $ makeNonsenseAST $ ASTFunction $ fn2 where
            fn2 _ AST { astNode = ASTInteger b } =
                return $ makeNonsenseAST $ ASTInteger $ a - b
            fn2 _ ast2 = throwL $ argError2 name ast1 ast2
    fn1 _ ast1@AST { astNode = ASTDouble a } =
        return $ makeNonsenseAST $ ASTFunction $ fn2 where
            fn2 _ AST { astNode = ASTDouble b } =
                return $ makeNonsenseAST $ ASTDouble $ a - b
            fn2 _ ast2 = throwL $ argError2 name ast1 ast2
    fn1 _ ast1 = throwL $ argError1 name ast1

builtinMultiply2 :: (String, AST)
builtinMultiply2 = (name, makeNonsenseAST $ ASTFunction fn1) where
    name = "*"
    fn1 _ ast1@AST { astNode = ASTInteger a } =
        return $ makeNonsenseAST $ ASTFunction $ fn2 where
            fn2 _ AST { astNode = ASTInteger b } =
                return $ makeNonsenseAST $ ASTInteger $ a * b
            fn2 _ ast2 = throwL $ argError2 name ast1 ast2
    fn1 _ ast1@AST { astNode = ASTDouble a } =
        return $ makeNonsenseAST $ ASTFunction $ fn2 where
            fn2 _ AST { astNode = ASTDouble b } =
                return $ makeNonsenseAST $ ASTDouble $ a * b
            fn2 _ ast2 = throwL $ argError2 name ast1 ast2
    fn1 _ ast1 = throwL $ argError1 name ast1

builtinDivide2 :: (String, AST)
builtinDivide2 = (name, makeNonsenseAST $ ASTFunction fn1) where
    name = "/"
    fn1 _ ast1@AST { astNode = ASTInteger a } =
        return $ makeNonsenseAST $ ASTFunction $ fn2 where
            fn2 _ AST { astNode = ASTInteger b } =
             do when (b == 0) $ throwL $ "division by zero"
                return $ makeNonsenseAST $ ASTInteger $ a `div` b
            fn2 _ ast2 = throwL $ argError2 name ast1 ast2
    fn1 _ ast1@AST { astNode = ASTDouble a } =
        return $ makeNonsenseAST $ ASTFunction $ fn2 where
            fn2 _ AST { astNode = ASTDouble b } =
             do when (b == 0) $ throwL $ "division by zero"
                return $ makeNonsenseAST $ ASTDouble $ a / b
            fn2 _ ast2 = throwL $ argError2 name ast1 ast2
    fn1 _ ast1 = throwL $ argError1 name ast1

builtinEq2 :: (String, AST)
builtinEq2 = (name, makeNonsenseAST $ ASTFunction fn1) where
    name = "eq?"
    fn1 _ ast1 =
        return $ makeNonsenseAST $ ASTFunction $ fn2 where
            fn2 _ ast2 = return $ makeNonsenseAST $ ASTBoolean $ ast1 == ast2

builtinLt2 :: (String, AST)
builtinLt2 = (name, makeNonsenseAST $ ASTFunction fn1) where
    name = "lt?"
    fn1 _ ast1 =
        return $ makeNonsenseAST $ ASTFunction $ fn2 where
            fn2 _ ast2 = return $ makeNonsenseAST $ ASTBoolean $ ast1 <= ast2

builtinFloor :: (String, AST)
builtinFloor = (name, makeNonsenseAST $ ASTFunction fn1) where
    name = "floor"
    fn1 _ AST { astNode = ASTDouble dbl } = return $ makeNonsenseAST $ ASTInteger $ floor dbl
    fn1 _ ast = throwL $ argError1 name ast

builtinToDouble :: (String, AST)
builtinToDouble = (name, makeNonsenseAST $ ASTFunction fn1) where
    name = "to-double"
    fn1 _ AST { astNode = ASTInteger int } = return $ makeNonsenseAST $ ASTDouble $ fromIntegral int
    fn1 _ ast = throwL $ argError1 name ast

builtinFmt :: (String, AST)
builtinFmt = (name, makeNonsenseAST $ ASTFunction fn1) where
    name = "fmt"
    fn1 _ ast1@AST { astNode = ASTString str } =
        return $ makeNonsenseAST $ ASTFunction $ fn2 where
            fn2 _ AST { astNode = ASTVector replacements } =
                return $ makeNonsenseAST $ ASTString $
                    T.unpack $ replaceAll (0 :: Int) replacements (T.pack str)
            fn2 _ ast2 = throwL $ argError2 name ast1 ast2
    fn1 _ ast1 = throwL $ argError1 name ast1
    replaceAll _ [] text = text
    replaceAll n (x:xs) text =
        let text' = T.replace (T.pack $ "{" ++ show n ++ "}") (T.pack $ show x) text
         in replaceAll (n + 1) xs text'

builtinHead :: (String, AST)
builtinHead = (name, makeNonsenseAST $ ASTFunction fn1) where
    name = "head"
    fn1 _ AST { astNode = ASTVector vec } =
     do when (length vec == 0) $ throwL $ name ++ " of empty vector"
        return $ head vec
    fn1 _ ast = throwL $ argError1 name ast

builtinTail :: (String, AST)
builtinTail = (name, makeNonsenseAST $ ASTFunction fn1) where
    name = "tail"
    fn1 _ AST { astNode = ASTVector vec } =
     do when (length vec == 0) $ throwL $ name ++ " of empty vector"
        return $ makeNonsenseAST $ ASTVector $ tail vec
    fn1 _ ast = throwL $ argError1 name ast

builtinSubstr :: (String, AST)
builtinSubstr = (name, makeNonsenseAST $ ASTFunction fn1) where
    name = "substr"
    fn1 _ ast1@AST { astNode = ASTInteger at } =
        return $ makeNonsenseAST $ ASTFunction $ fn2 where
            fn2 _ ast2@AST { astNode = ASTInteger len } =
                return $ makeNonsenseAST $ ASTFunction $ fn3 where
                    fn3 _ AST { astNode = ASTString str } =
                        return $ makeNonsenseAST $ ASTString $ drop at .> take len $ str
                    fn3 _ ast3 = throwL $ argError3 name ast1 ast2 ast3
            fn2 _ ast2 = throwL $ argError2 name ast1 ast2
    fn1 _ ast1 = throwL $ argError1 name ast1

builtinPrepend :: (String, AST)
builtinPrepend = (name, makeNonsenseAST $ ASTFunction fn1) where
    name = "prepend"
    fn1 _ ast1 =
        return $ makeNonsenseAST $ ASTFunction $ fn2 where
            fn2 _ AST { astNode = ASTVector vec } =
                return $ makeNonsenseAST $ ASTVector $ ast1 : vec
            fn2 _ ast2 = throwL $ argError2 name ast1 ast2

builtinPrint :: (String, AST)
builtinPrint = (name, makeNonsenseAST $ ASTFunction fn1) where
    name = "print!"
    fn1 _ AST { astNode = ASTString str } =
     do liftIO $ putStr $ str
        return $ makeNonsenseAST ASTUnit
    fn1 _ ast = throwL $ argError1 name ast

builtinConcat :: (String, AST)
builtinConcat = (name, makeNonsenseAST $ ASTFunction fn1) where
    name = "concat"
    fn1 _ ast1@AST { astNode = ASTString str1 } =
        return $ makeNonsenseAST $ ASTFunction $ fn2 where
            fn2 _ AST { astNode = ASTString str2 } =
                return $ makeNonsenseAST $ ASTString $ str1 ++ str2
            fn2 _ ast2 = throwL $ argError2 name ast1 ast2
    fn1 _ ast1 = throwL $ argError1 name ast1

builtinFatal :: (String, AST)
builtinFatal = (name, makeNonsenseAST $ ASTFunction fn1) where
    name = "fatal"
    fn1 _ AST { astNode = ASTString str } =
        throwL $ str
    fn1 _ ast =
        throwL $ argError1 name ast
