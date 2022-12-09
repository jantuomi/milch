{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Builtins where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Text.Read as TR
import qualified Data.List as L
import qualified Data.Bifunctor as B
import Control.Monad.State
import Utils

builtinEnv :: Env
builtinEnv = M.fromList $ map (B.second Regular) [
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
    builtinSortByFirst,
    -- string operations
    builtinSubstr,
    builtinStrToVec,
    builtinLen,
    -- vector & string operations
    builtinConcat,
    -- filesystem operations
    builtinReadFile,
    builtinWriteFile,
    builtinAppendFile,
    -- special
    builtinPrint,
    builtinTry,
    ("unit", makeNonsenseAST ASTUnit),
    ("_", makeNonsenseAST ASTHole),
    ("otherwise", makeNonsenseAST ASTHole),
    builtinFatal,
    builtinKind,
    -- atom
    builtinAtom,
    builtinAtomUpdate,
    builtinAtomGet,
    -- reserved keywords
    reservedKeyword "\\",
    reservedKeyword "\\!",
    reservedKeyword "let",
    reservedKeyword "match",
    reservedKeyword "Debug/env",
    reservedKeyword "import",
    reservedKeyword "record",
    reservedKeyword "do"
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
reservedKeyword name = (name, makeNonsenseAST $ ASTFunction Pure fn1) where
    fn1 ast1 = throwL (astPos ast1, "unreachable: " ++ name ++ " is a reserved word")

-- BUILTINS

builtinAdd2 :: (String, AST)
builtinAdd2 = (name, makeNonsenseAST $ ASTFunction Pure fn1) where
    name = "+"
    fn1 ast1@AST { an = ASTInteger a } =
        return $ makeNonsenseAST $ ASTFunction Pure $ fn2 where
            fn2 AST { an = ASTInteger b } =
                return $ makeNonsenseAST $ ASTInteger $ a + b
            fn2 ast2 = throwL (astPos ast2, argError2 name ast1 ast2)
    fn1 ast1@AST { an = ASTDouble a } =
        return $ makeNonsenseAST $ ASTFunction Pure $ fn2 where
            fn2 AST { an = ASTDouble b } =
                return $ makeNonsenseAST $ ASTDouble $ a + b
            fn2 ast2 = throwL (astPos ast2, argError2 name ast1 ast2)
    fn1 ast1 = throwL (astPos ast1, argError1 name ast1)

builtinSubtract2 :: (String, AST)
builtinSubtract2 = (name, makeNonsenseAST $ ASTFunction Pure fn1) where
    name = "-"
    fn1 ast1@AST { an = ASTInteger a } =
        return $ makeNonsenseAST $ ASTFunction Pure $ fn2 where
            fn2 AST { an = ASTInteger b } =
                return $ makeNonsenseAST $ ASTInteger $ a - b
            fn2 ast2 = throwL (astPos ast2, argError2 name ast1 ast2)
    fn1 ast1@AST { an = ASTDouble a } =
        return $ makeNonsenseAST $ ASTFunction Pure $ fn2 where
            fn2 AST { an = ASTDouble b } =
                return $ makeNonsenseAST $ ASTDouble $ a - b
            fn2 ast2 = throwL (astPos ast2, argError2 name ast1 ast2)
    fn1 ast1 = throwL (astPos ast1, argError1 name ast1)

builtinMultiply2 :: (String, AST)
builtinMultiply2 = (name, makeNonsenseAST $ ASTFunction Pure fn1) where
    name = "*"
    fn1 ast1@AST { an = ASTInteger a } =
        return $ makeNonsenseAST $ ASTFunction Pure $ fn2 where
            fn2 AST { an = ASTInteger b } =
                return $ makeNonsenseAST $ ASTInteger $ a * b
            fn2 ast2 = throwL (astPos ast2, argError2 name ast1 ast2)
    fn1 ast1@AST { an = ASTDouble a } =
        return $ makeNonsenseAST $ ASTFunction Pure $ fn2 where
            fn2 AST { an = ASTDouble b } =
                return $ makeNonsenseAST $ ASTDouble $ a * b
            fn2 ast2 = throwL (astPos ast2, argError2 name ast1 ast2)
    fn1 ast1 = throwL (astPos ast1, argError1 name ast1)

builtinDivide2 :: (String, AST)
builtinDivide2 = (name, makeNonsenseAST $ ASTFunction Pure fn1) where
    name = "/"
    fn1 ast1@AST { an = ASTInteger a } =
        return $ makeNonsenseAST $ ASTFunction Pure $ fn2 where
            fn2 ast2@AST { an = ASTInteger b } =
             do when (b == 0) $ throwL (astPos ast2, "division by zero")
                return $ makeNonsenseAST $ ASTInteger $ a `div` b
            fn2 ast2 = throwL (astPos ast2, argError2 name ast1 ast2)
    fn1 ast1@AST { an = ASTDouble a } =
        return $ makeNonsenseAST $ ASTFunction Pure $ fn2 where
            fn2 ast2@AST { an = ASTDouble b } =
             do when (b == 0) $ throwL (astPos ast2, "division by zero")
                return $ makeNonsenseAST $ ASTDouble $ a / b
            fn2 ast2 = throwL (astPos ast2, argError2 name ast1 ast2)
    fn1 ast1 = throwL (astPos ast1, argError1 name ast1)

builtinEq2 :: (String, AST)
builtinEq2 = (name, makeNonsenseAST $ ASTFunction Pure fn1) where
    name = "eq?"
    fn1 ast1 =
        return $ makeNonsenseAST $ ASTFunction Pure $ fn2 where
            fn2 ast2 = return $ makeNonsenseAST $ ASTBoolean $ ast1 == ast2

builtinLt2 :: (String, AST)
builtinLt2 = (name, makeNonsenseAST $ ASTFunction Pure fn1) where
    name = "lt?"
    fn1 ast1 =
        return $ makeNonsenseAST $ ASTFunction Pure $ fn2 where
            fn2 ast2 = return $ makeNonsenseAST $ ASTBoolean $ ast1 < ast2

builtinFloor :: (String, AST)
builtinFloor = (name, makeNonsenseAST $ ASTFunction Pure fn1) where
    name = "floor"
    fn1 AST { an = ASTDouble dbl } = return $ makeNonsenseAST $ ASTInteger $ floor dbl
    fn1 ast1 = throwL (astPos ast1, argError1 name ast1)

builtinParseInt :: (String, AST)
builtinParseInt = (name, makeNonsenseAST $ ASTFunction Pure fn1) where
    name = "parse-int"
    fn1 ast1@AST { an = ASTString str } =
        case (TR.readMaybe str) of
            Just val -> return $ makeNonsenseAST $ ASTInteger $ val
            Nothing -> throwL (astPos ast1, argError1 name ast1)
    fn1 ast1 = throwL (astPos ast1, argError1 name ast1)

builtinToDouble :: (String, AST)
builtinToDouble = (name, makeNonsenseAST $ ASTFunction Pure fn1) where
    name = "to-double"
    fn1 AST { an = ASTInteger int } = return $ makeNonsenseAST $ ASTDouble $ fromIntegral int
    fn1 ast1 = throwL (astPos ast1, argError1 name ast1)

builtinFmt :: (String, AST)
builtinFmt = (name, makeNonsenseAST $ ASTFunction Pure fn1) where
    name = "fmt"
    fn1 ast1@AST { an = ASTString str } =
        return $ makeNonsenseAST $ ASTFunction Pure $ fn2 where
            fn2 AST { an = ASTVector replacements } =
                return $ makeNonsenseAST $ ASTString $
                    T.unpack $ replaceAll (0 :: Int) replacements (T.pack str)
            fn2 ast2 = throwL (astPos ast2, argError2 name ast1 ast2)
    fn1 ast1 = throwL (astPos ast1, argError1 name ast1)
    replaceAll :: Int -> [AST] -> T.Text -> T.Text
    replaceAll _ [] text = text
    replaceAll n (x:xs) text =
        let xRepr = case x of
                AST { an = ASTString s } -> s
                _ -> show x
            text' = T.replace (T.pack $ "{" ++ show n ++ "}") (T.pack $ xRepr) text
         in replaceAll (n + 1) xs text'

builtinHead :: (String, AST)
builtinHead = (name, makeNonsenseAST $ ASTFunction Pure fn1) where
    name = "head"
    fn1 ast1@AST { an = ASTVector vec } =
     do when (length vec == 0) $ throwL (astPos ast1, name ++ " of empty vector")
        return $ head vec
    fn1 ast1 = throwL (astPos ast1, argError1 name ast1)

builtinTail :: (String, AST)
builtinTail = (name, makeNonsenseAST $ ASTFunction Pure fn1) where
    name = "tail"
    fn1 ast1@AST { an = ASTVector vec } =
     do when (length vec == 0) $ throwL (astPos ast1, name ++ " of empty vector")
        return $ makeNonsenseAST $ ASTVector $ tail vec
    fn1 ast1 = throwL (astPos ast1, argError1 name ast1)

builtinSubstr :: (String, AST)
builtinSubstr = (name, makeNonsenseAST $ ASTFunction Pure fn1) where
    name = "substr"
    fn1 ast1@AST { an = ASTInteger atInteger } =
        return $ makeNonsenseAST $ ASTFunction Pure $ fn2 where
            fn2 ast2@AST { an = ASTInteger lenInteger } =
                return $ makeNonsenseAST $ ASTFunction Pure $ fn3 where
                    len = fromIntegral lenInteger :: Int
                    at = fromIntegral atInteger :: Int
                    fn3 AST { an = ASTString str } =
                        return $ makeNonsenseAST $ ASTString $ drop at .> take len $ str
                    fn3 ast3 = throwL (astPos ast3, argError3 name ast1 ast2 ast3)
            fn2 ast2 = throwL (astPos ast2, argError2 name ast1 ast2)
    fn1 ast1 = throwL (astPos ast1, argError1 name ast1)

builtinStrToVec :: (String, AST)
builtinStrToVec = (name, makeNonsenseAST $ ASTFunction Pure fn1) where
    name = "to-vec"
    fn1 AST { an = ASTString str } =
        str $> map (\c -> [c])
            .> map (makeNonsenseAST . ASTString)
            .> (makeNonsenseAST . ASTVector)
            .> return
    fn1 ast1 = throwL (astPos ast1, argError1 name ast1)

builtinPrepend :: (String, AST)
builtinPrepend = (name, makeNonsenseAST $ ASTFunction Pure fn1) where
    name = "prepend"
    fn1 ast1 =
        return $ makeNonsenseAST $ ASTFunction Pure $ fn2 where
            fn2 AST { an = ASTVector vec } =
                return $ makeNonsenseAST $ ASTVector $ ast1 : vec
            fn2 ast2 = throwL (astPos ast2, argError2 name ast1 ast2)

builtinPrint :: (String, AST)
builtinPrint = (name, makeNonsenseAST $ ASTFunction Impure fn1) where
    name = "print!"
    fn1 AST { an = ASTString str } =
     do liftIO $ putStr $ str
        return $ makeNonsenseAST ASTUnit
    fn1 ast1 = throwL (astPos ast1, argError1 name ast1)

builtinConcat :: (String, AST)
builtinConcat = (name, makeNonsenseAST $ ASTFunction Pure fn1) where
    name = "concat"
    fn1 ast1@AST { an = ASTString str1 } =
        return $ makeNonsenseAST $ ASTFunction Pure $ fn2 where
            fn2 AST { an = ASTString str2 } =
                return $ makeNonsenseAST $ ASTString $ str1 ++ str2
            fn2 ast2 = throwL (astPos ast2, argError2 name ast1 ast2)
    fn1 ast1 = throwL (astPos ast1, argError1 name ast1)

builtinLen :: (String, AST)
builtinLen = (name, makeNonsenseAST $ ASTFunction Pure fn1) where
    name = "len"
    fn1 AST { an = ASTString str } =
        return $ makeNonsenseAST $ ASTInteger $ fromIntegral $ length str
    fn1 ast1 = throwL (astPos ast1, argError1 name ast1)

builtinFatal :: (String, AST)
builtinFatal = (name, makeNonsenseAST $ ASTFunction Impure fn1) where
    name = "fatal!"
    fn1 ast1@AST { an = ASTString str } =
        throwL (astPos ast1, str)
    fn1 ast1 =
        throwL (astPos ast1, argError1 name ast1)

builtinKind :: (String, AST)
builtinKind = (name, makeNonsenseAST $ ASTFunction Pure fn1) where
    name = "kind"
    fn1 AST { an = ASTRecord tagHash identifier _} =
        return $ makeNonsenseAST $ ASTTag tagHash identifier
    fn1 ast1 = throwL (astPos ast1, argError1 name ast1)

builtinReadFile :: (String, AST)
builtinReadFile = (name, makeNonsenseAST $ ASTFunction Impure fn1) where
    name = "read-file!"
    fn1 ast1@AST { an = ASTString filePath } = do
        contentsM <- liftIO $ safeReadFile filePath
        case contentsM of
            Just contents -> return $ makeNonsenseAST $ ASTString contents
            Nothing -> throwL (astPos ast1, "failed to read file: " ++ filePath)
    fn1 ast1 = throwL (astPos ast1, argError1 name ast1)

builtinWriteFile :: (String, AST)
builtinWriteFile = (name, makeNonsenseAST $ ASTFunction Pure fn1) where
    name = "write-file!"
    fn1 ast1@AST { an = ASTString filePath } =
        return $ makeNonsenseAST $ ASTFunction Impure $ fn2 where
            fn2 AST { an = ASTString content } = do
                resultM <- liftIO $ safeWriteFile filePath content
                case resultM of
                    Just () -> return $ makeNonsenseAST $ ASTUnit
                    Nothing -> throwL (astPos ast1, "failed to write file: " ++ filePath)
            fn2 ast2 = throwL (astPos ast2, argError2 name ast1 ast2)
    fn1 ast1 = throwL (astPos ast1, argError1 name ast1)

builtinAppendFile :: (String, AST)
builtinAppendFile = (name, makeNonsenseAST $ ASTFunction Pure fn1) where
    name = "append-file!"
    fn1 ast1@AST { an = ASTString filePath } =
        return $ makeNonsenseAST $ ASTFunction Impure $ fn2 where
            fn2 AST { an = ASTString content } = do
                resultM <- liftIO $ safeAppendFile filePath content
                case resultM of
                    Just () -> return $ makeNonsenseAST $ ASTUnit
                    Nothing -> throwL (astPos ast1, "failed to append to file: " ++ filePath)
            fn2 ast2 = throwL (astPos ast2, argError2 name ast1 ast2)
    fn1 ast1 = throwL (astPos ast1, argError1 name ast1)

builtinSortByFirst :: (String, AST)
builtinSortByFirst = (name, makeNonsenseAST $ ASTFunction Pure fn1) where
    name = "sort-by-first"
    fn1 ast1@AST { an = ASTVector elems } = do
        pairs <- mapM elemToPair elems
        let sorted = L.sortBy (\(a, _) (b, _) -> compare a b) pairs
        let sortedASTS = map (\(k, v) -> makeNonsenseAST $
                ASTVector [makeNonsenseAST $ ASTInteger k, v]) sorted
        return $ makeNonsenseAST $ ASTVector sortedASTS where
            itemsToPair [AST { an = ASTInteger k }, v] =
                return $ (k, v)
            itemsToPair items = throwL (astPos ast1,
                "invalid element in vector supplied to sort-by-first: " ++ show items)
            elemToPair AST { an = ASTVector items } =
                itemsToPair items
            elemToPair ast2 = throwL (astPos ast2, argError1 name ast1)
    fn1 ast1 = throwL (astPos ast1, argError1 name ast1)

builtinTry :: (String, AST)
builtinTry = (name, makeNonsenseAST $ ASTFunction Pure fn1) where
    name = "try!"
    fn1 ast1@AST { an = ASTFunction Pure catchFn } =
        return $ makeNonsenseAST $ ASTFunction Pure $ fn2 where
            fn2 ast2@AST { an = ASTFunction _ tryFn } =
                return $ makeNonsenseAST $ ASTFunction Impure $ fn3 where
                    fn3 ast3 = do
                        s <- get
                        let tryRet = tryFn ast3
                        tryRetE <- liftIO $ runL s tryRet
                        case tryRetE of
                            Right (val, state') -> do
                                put state'
                                return val
                            Left (LException stack) -> do
                                let es = snd $ last stack
                                catchFn $ ast2 { an = ASTString $ es }
            fn2 ast2 = throwL (astPos ast2, argError2 name ast1 ast2)
    fn1 ast1 = throwL (astPos ast1, argError1 name ast1)

builtinAtom :: (String, AST)
builtinAtom = (name, makeNonsenseAST $ ASTFunction Impure fn1) where
    name = "atom!"
    fn1 ast1 = createAtom ast1

builtinAtomUpdate :: (String, AST)
builtinAtomUpdate = (name, makeNonsenseAST $ ASTFunction Pure fn1) where
    name = "atom-update!"
    fn1 ast1@AST { an = ASTFunction Pure updateFn } =
        return $ makeNonsenseAST $ ASTFunction Impure $ fn2 where
            fn2 ast2@AST { an = ASTAtom ref } = do
                atomMap <- getAtomMap
                case (M.lookup ref atomMap) of
                    Just hit -> do
                        mapped <- updateFn hit
                        insertAtomMap ref mapped
                        return ast2
                    Nothing ->
                        throwL (astPos ast2, "invalid atom reference: " ++ show ref)
            fn2 ast2 = throwL (astPos ast2, argError2 name ast1 ast2)
    fn1 ast1 = throwL (astPos ast1, argError1 name ast1)

builtinAtomGet :: (String, AST)
builtinAtomGet = (name, makeNonsenseAST $ ASTFunction Impure fn1) where
    name = "atom-get!"
    fn1 ast1@AST { an = ASTAtom ref } = do
        atomMap <- getAtomMap
        case (M.lookup ref atomMap) of
            Just hit -> return hit
            Nothing ->
                throwL (astPos ast1, "invalid atom reference: " ++ show ref)
    fn1 ast1 = throwL (astPos ast1, argError1 name ast1)
