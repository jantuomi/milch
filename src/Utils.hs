{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Utils where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Char as C

-- TYPES

newtype LException = LException String
data Config = Config {
    configScriptFileName :: Maybe String,
    configVerboseMode :: Bool,
    configShowHelp :: Bool,
    configPrintEvaled :: Bool,
    configPrintCallStack :: Bool
}

type LContext a = ReaderT Config (ExceptT LException IO) a

runL :: Config -> LContext a -> IO (Either LException a)
runL config lc = runExceptT $ runReaderT lc config

type Env = M.Map String AST

type LFunction = (Env -> AST -> LContext AST)

data AST
    = ASTInteger Int
    | ASTDouble Double
    | ASTSymbol String
    | ASTBoolean Bool
    | ASTString String
    | ASTVector [AST]
    | ASTFunctionCall [AST]
    | ASTHashMap (M.Map AST AST)
    | ASTFunction LFunction
    | ASTUnit
    | ASTHole

instance (Show AST) where
    show (ASTInteger n) = show n
    show (ASTDouble n) = show n
    show (ASTSymbol s) = s
    show (ASTBoolean b) = show b $> map C.toLower
    show (ASTString s) = show s
    show (ASTVector v) = "[" ++ L.intercalate " " (map show v) ++ "]"
    show (ASTFunctionCall v) = "(" ++ L.intercalate " " (map show v) ++ ")"
    show (ASTHashMap m) =
        let flattenMap = M.assocs .> L.concatMap (\(k, v) -> [k, v])
         in "{" ++ L.intercalate " " (map show $ flattenMap m) ++ "}"
    show (ASTFunction _) = "<fn>"
    show ASTUnit = "<unit>"
    show ASTHole = "<hole>"

instance (Eq AST) where
    ASTInteger a == ASTInteger b = a == b
    ASTDouble a == ASTDouble b = a == b
    ASTSymbol a == ASTSymbol b = a == b
    ASTBoolean a == ASTBoolean b = a == b
    ASTString a == ASTString b = a == b
    ASTVector a == ASTVector b = a == b
    ASTFunctionCall a == ASTFunctionCall b = a == b
    ASTHashMap a == ASTHashMap b = a == b
    ASTUnit == ASTUnit = True
    ASTHole == _ = True
    _ == ASTHole = True
    _ == _ = False

instance (Ord AST) where
    ASTInteger a <= ASTInteger b = a <= b
    ASTDouble a <= ASTDouble b = a <= b
    ASTSymbol a <= ASTSymbol b = a <= b
    ASTBoolean a <= ASTBoolean b = a <= b
    ASTString a <= ASTString b = a <= b
    ASTVector a <= ASTVector b = a <= b
    ASTFunctionCall a <= ASTFunctionCall b = a <= b
    ASTHashMap a <= ASTHashMap b = a <= b
    ASTUnit <= ASTUnit = True
    ASTHole <= _ = True
    _ <= ASTHole = True
    _ <= _ = False

assertIsASTFunction :: AST -> LContext AST
assertIsASTFunction ast = case ast of
    (ASTFunction _) -> return ast
    _ -> throwL $ show ast ++ " is not a function"

assertIsASTInteger :: AST -> LContext AST
assertIsASTInteger ast = case ast of
    (ASTInteger _) -> return ast
    _ -> throwL $ show ast ++ " is not an integer"

assertIsASTSymbol :: AST -> LContext AST
assertIsASTSymbol ast = case ast of
    (ASTSymbol _) -> return ast
    _ -> throwL $ show ast ++ " is not a symbol"

assertIsASTVector :: AST -> LContext AST
assertIsASTVector ast = case ast of
    (ASTVector _) -> return ast
    _ -> throwL $ show ast ++ " is not a vector"

assertIsASTString :: AST -> LContext AST
assertIsASTString ast = case ast of
    (ASTString _) -> return ast
    _ -> throwL $ show ast ++ " is not a string"

assertIsASTFunctionCall :: AST -> LContext AST
assertIsASTFunctionCall ast = case ast of
    (ASTFunctionCall _) -> return ast
    _ -> throwL $ show ast ++ " is not a function call or body"

-- UTILS

(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)
($>) :: b -> (b -> c) -> c
($>) = flip ($)

infixr 6 $>

oddElems :: [a] -> [a]
oddElems [] = []
oddElems (x:xs) = x:evenElems xs

evenElems :: [a] -> [a]
evenElems [] = []
evenElems (_:xs) = oddElems xs

throwL :: String -> LContext a
throwL s = throwError $ LException s

asPairsM :: [a] -> LContext [(a, a)]
asPairsM [] = return []
asPairsM (a:b:rest) = do
    restPaired <- asPairsM rest
    return $ (a, b) : restPaired
asPairsM _ = throwL "odd number of elements to pair up"

asPairs :: [a] -> [(a, a)]
asPairs [] = []
asPairs (a:b:rest) =
    let restPaired = asPairs rest
     in (a, b) : restPaired
asPairs _ = error "odd number of elements to pair up"
