{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Builtins where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Text.Read as TR
import Control.Monad.Except
import Utils

builtinEnv :: EnvMap
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
    builtinParseInt,
    -- vector operations
    builtinHead,
    builtinTail,
    builtinPrepend,
    -- string operations
    builtinSubstr,
    builtinStrToVec,
    -- vector & string operations
    builtinConcat,
    -- special
    builtinPrint,
    ("unit", makeNonsenseAST ASTUnit),
    ("_", makeNonsenseAST ASTHole),
    ("otherwise", makeNonsenseAST ASTHole),
    builtinFatal,
    builtinKind,
    -- reserved keywords
    reservedKeyword "\\",
    reservedKeyword "let!",
    reservedKeyword "match",
    reservedKeyword "env!",
    reservedKeyword "record!"
    ]

argError1 :: String -> AST -> String
argError1 fn arg1 =
    "invalid argument to " ++ fn ++ ": " ++ show arg1

argError2 :: String -> AST -> AST -> String
argError2 fn arg1 arg2 =
    "invalid arguments to " ++ fn ++ ": " ++ show arg1 ++ ", " ++ show arg2

argError3 :: String -> AST -> AST -> AST -> String
argError3 fn arg1 arg2 arg3 =
    "invalid arguments to " ++ fn ++ ": " ++ show arg1 ++ ", " ++ show arg2 ++ ", " ++ show arg3

reservedKeyword :: String -> (String, AST)
reservedKeyword name = (name, makeNonsenseAST $ ASTFunction True fn1) where
    fn1 ast1 = throwL (astPos ast1) $ "unreachable: " ++ name ++ " is a reserved word"

-- BUILTINS

builtinAdd2 :: (String, AST)
builtinAdd2 = (name, makeNonsenseAST $ ASTFunction True fn1) where
    name = "+"
    fn1 ast1@AST { astNode = ASTInteger a } =
        return $ makeNonsenseAST $ ASTFunction True $ fn2 where
            fn2 AST { astNode = ASTInteger b } =
                return $ makeNonsenseAST $ ASTInteger $ a + b
            fn2 ast2 = throwL (astPos ast2) $ argError2 name ast1 ast2
    fn1 ast1@AST { astNode = ASTDouble a } =
        return $ makeNonsenseAST $ ASTFunction True $ fn2 where
            fn2 AST { astNode = ASTDouble b } =
                return $ makeNonsenseAST $ ASTDouble $ a + b
            fn2 ast2 = throwL (astPos ast2) $ argError2 name ast1 ast2
    fn1 ast1 = throwL (astPos ast1) $ argError1 name ast1

builtinSubtract2 :: (String, AST)
builtinSubtract2 = (name, makeNonsenseAST $ ASTFunction True fn1) where
    name = "-"
    fn1 ast1@AST { astNode = ASTInteger a } =
        return $ makeNonsenseAST $ ASTFunction True $ fn2 where
            fn2 AST { astNode = ASTInteger b } =
                return $ makeNonsenseAST $ ASTInteger $ a - b
            fn2 ast2 = throwL (astPos ast2) $ argError2 name ast1 ast2
    fn1 ast1@AST { astNode = ASTDouble a } =
        return $ makeNonsenseAST $ ASTFunction True $ fn2 where
            fn2 AST { astNode = ASTDouble b } =
                return $ makeNonsenseAST $ ASTDouble $ a - b
            fn2 ast2 = throwL (astPos ast2) $ argError2 name ast1 ast2
    fn1 ast1 = throwL (astPos ast1) $ argError1 name ast1

builtinMultiply2 :: (String, AST)
builtinMultiply2 = (name, makeNonsenseAST $ ASTFunction True fn1) where
    name = "*"
    fn1 ast1@AST { astNode = ASTInteger a } =
        return $ makeNonsenseAST $ ASTFunction True $ fn2 where
            fn2 AST { astNode = ASTInteger b } =
                return $ makeNonsenseAST $ ASTInteger $ a * b
            fn2 ast2 = throwL (astPos ast2) $ argError2 name ast1 ast2
    fn1 ast1@AST { astNode = ASTDouble a } =
        return $ makeNonsenseAST $ ASTFunction True $ fn2 where
            fn2 AST { astNode = ASTDouble b } =
                return $ makeNonsenseAST $ ASTDouble $ a * b
            fn2 ast2 = throwL (astPos ast2) $ argError2 name ast1 ast2
    fn1 ast1 = throwL (astPos ast1) $ argError1 name ast1

builtinDivide2 :: (String, AST)
builtinDivide2 = (name, makeNonsenseAST $ ASTFunction True fn1) where
    name = "/"
    fn1 ast1@AST { astNode = ASTInteger a } =
        return $ makeNonsenseAST $ ASTFunction True $ fn2 where
            fn2 ast2@AST { astNode = ASTInteger b } =
             do when (b == 0) $ throwL (astPos ast2) $ "division by zero"
                return $ makeNonsenseAST $ ASTInteger $ a `div` b
            fn2 ast2 = throwL (astPos ast2) $ argError2 name ast1 ast2
    fn1 ast1@AST { astNode = ASTDouble a } =
        return $ makeNonsenseAST $ ASTFunction True $ fn2 where
            fn2 ast2@AST { astNode = ASTDouble b } =
             do when (b == 0) $ throwL (astPos ast2) $ "division by zero"
                return $ makeNonsenseAST $ ASTDouble $ a / b
            fn2 ast2 = throwL (astPos ast2) $ argError2 name ast1 ast2
    fn1 ast1 = throwL (astPos ast1) $ argError1 name ast1

builtinEq2 :: (String, AST)
builtinEq2 = (name, makeNonsenseAST $ ASTFunction True fn1) where
    name = "eq?"
    fn1 ast1 =
        return $ makeNonsenseAST $ ASTFunction True $ fn2 where
            fn2 ast2 = return $ makeNonsenseAST $ ASTBoolean $ ast1 == ast2

builtinLt2 :: (String, AST)
builtinLt2 = (name, makeNonsenseAST $ ASTFunction True fn1) where
    name = "lt?"
    fn1 ast1 =
        return $ makeNonsenseAST $ ASTFunction True $ fn2 where
            fn2 ast2 = return $ makeNonsenseAST $ ASTBoolean $ ast1 < ast2

builtinFloor :: (String, AST)
builtinFloor = (name, makeNonsenseAST $ ASTFunction True fn1) where
    name = "floor"
    fn1 AST { astNode = ASTDouble dbl } = return $ makeNonsenseAST $ ASTInteger $ floor dbl
    fn1 ast1 = throwL (astPos ast1) $ argError1 name ast1

builtinParseInt :: (String, AST)
builtinParseInt = (name, makeNonsenseAST $ ASTFunction True fn1) where
    name = "parse-int"
    fn1 ast1@AST { astNode = ASTString str } =
        case (TR.readMaybe str) of
            Just val -> return $ makeNonsenseAST $ ASTInteger $ val
            Nothing -> throwL (astPos ast1) $ argError1 name ast1
    fn1 ast1 = throwL (astPos ast1) $ argError1 name ast1

builtinToDouble :: (String, AST)
builtinToDouble = (name, makeNonsenseAST $ ASTFunction True fn1) where
    name = "to-double"
    fn1 AST { astNode = ASTInteger int } = return $ makeNonsenseAST $ ASTDouble $ fromIntegral int
    fn1 ast1 = throwL (astPos ast1) $ argError1 name ast1

builtinFmt :: (String, AST)
builtinFmt = (name, makeNonsenseAST $ ASTFunction True fn1) where
    name = "fmt"
    fn1 ast1@AST { astNode = ASTString str } =
        return $ makeNonsenseAST $ ASTFunction True $ fn2 where
            fn2 AST { astNode = ASTVector replacements } =
                return $ makeNonsenseAST $ ASTString $
                    T.unpack $ replaceAll (0 :: Int) replacements (T.pack str)
            fn2 ast2 = throwL (astPos ast2) $ argError2 name ast1 ast2
    fn1 ast1 = throwL (astPos ast1) $ argError1 name ast1
    replaceAll :: Int -> [AST] -> T.Text -> T.Text
    replaceAll _ [] text = text
    replaceAll n (x:xs) text =
        let xRepr = case x of
                AST { astNode = ASTString s } -> s
                _ -> show x
            text' = T.replace (T.pack $ "{" ++ show n ++ "}") (T.pack $ xRepr) text
         in replaceAll (n + 1) xs text'

builtinHead :: (String, AST)
builtinHead = (name, makeNonsenseAST $ ASTFunction True fn1) where
    name = "head"
    fn1 ast1@AST { astNode = ASTVector vec } =
     do when (length vec == 0) $ throwL (astPos ast1) $ name ++ " of empty vector"
        return $ head vec
    fn1 ast1 = throwL (astPos ast1) $ argError1 name ast1

builtinTail :: (String, AST)
builtinTail = (name, makeNonsenseAST $ ASTFunction True fn1) where
    name = "tail"
    fn1 ast1@AST { astNode = ASTVector vec } =
     do when (length vec == 0) $ throwL (astPos ast1) $ name ++ " of empty vector"
        return $ makeNonsenseAST $ ASTVector $ tail vec
    fn1 ast1 = throwL (astPos ast1) $ argError1 name ast1

builtinSubstr :: (String, AST)
builtinSubstr = (name, makeNonsenseAST $ ASTFunction True fn1) where
    name = "substr"
    fn1 ast1@AST { astNode = ASTInteger at } =
        return $ makeNonsenseAST $ ASTFunction True $ fn2 where
            fn2 ast2@AST { astNode = ASTInteger len } =
                return $ makeNonsenseAST $ ASTFunction True $ fn3 where
                    fn3 AST { astNode = ASTString str } =
                        return $ makeNonsenseAST $ ASTString $ drop at .> take len $ str
                    fn3 ast3 = throwL (astPos ast3) $ argError3 name ast1 ast2 ast3
            fn2 ast2 = throwL (astPos ast2) $ argError2 name ast1 ast2
    fn1 ast1 = throwL (astPos ast1) $ argError1 name ast1

builtinStrToVec :: (String, AST)
builtinStrToVec = (name, makeNonsenseAST $ ASTFunction True fn1) where
    name = "to-vec"
    fn1 AST { astNode = ASTString str } =
        str $> map (\c -> [c])
            .> map (makeNonsenseAST . ASTString)
            .> (makeNonsenseAST . ASTVector)
            .> return
    fn1 ast1 = throwL (astPos ast1) $ argError1 name ast1

builtinPrepend :: (String, AST)
builtinPrepend = (name, makeNonsenseAST $ ASTFunction True fn1) where
    name = "prepend"
    fn1 ast1 =
        return $ makeNonsenseAST $ ASTFunction True $ fn2 where
            fn2 AST { astNode = ASTVector vec } =
                return $ makeNonsenseAST $ ASTVector $ ast1 : vec
            fn2 ast2 = throwL (astPos ast2) $ argError2 name ast1 ast2

builtinPrint :: (String, AST)
builtinPrint = (name, makeNonsenseAST $ ASTFunction False fn1) where
    name = "print!"
    fn1 AST { astNode = ASTString str } =
     do liftIO $ putStr $ str
        return $ makeNonsenseAST ASTUnit
    fn1 ast1 = throwL (astPos ast1) $ argError1 name ast1

builtinConcat :: (String, AST)
builtinConcat = (name, makeNonsenseAST $ ASTFunction True fn1) where
    name = "concat"
    fn1 ast1@AST { astNode = ASTString str1 } =
        return $ makeNonsenseAST $ ASTFunction True $ fn2 where
            fn2 AST { astNode = ASTString str2 } =
                return $ makeNonsenseAST $ ASTString $ str1 ++ str2
            fn2 ast2 = throwL (astPos ast2) $ argError2 name ast1 ast2
    fn1 ast1 = throwL (astPos ast1) $ argError1 name ast1

builtinFatal :: (String, AST)
builtinFatal = (name, makeNonsenseAST $ ASTFunction False fn1) where
    name = "fatal!"
    fn1 ast1@AST { astNode = ASTString str } =
        throwL (astPos ast1) $ str
    fn1 ast1 =
        throwL (astPos ast1) $ argError1 name ast1

builtinKind :: (String, AST)
builtinKind = (name, makeNonsenseAST $ ASTFunction True fn1) where
    name = "kind"
    fn1 AST { astNode = ASTRecord identifier _} =
        return $ makeNonsenseAST $ ASTString identifier
    fn1 ast1 = throwL (astPos ast1) $ argError1 name ast1
