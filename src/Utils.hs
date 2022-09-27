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

data Token = Token {
    tokenContent :: String,
    tokenRow :: Int,
    tokenColumn :: Int,
    tokenFileName :: String
}

instance (Eq Token) where
    Token { tokenContent = tc1 } == Token { tokenContent = tc2 } = tc1 == tc2

instance (Show Token) where
    show token = show $ tokenContent token

type Env = M.Map String AST

type LFunction = (Env -> AST -> LContext AST)

data ASTNode
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

data AST = AST {
    astNode :: ASTNode,
    astRow :: Int,
    astColumn :: Int,
    astFileName :: String
}

instance (Show AST) where
    show (AST { astNode = node }) = show node

instance (Show ASTNode) where
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
    AST { astNode = node1 } == AST { astNode = node2 } = node1 == node2

instance (Eq ASTNode) where
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
    AST { astNode = node1 } <= AST { astNode = node2 } = node1 <= node2

instance (Ord ASTNode) where
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
assertIsASTFunction ast@(AST { astNode = node }) = case node of
    (ASTFunction _) -> return ast
    _ -> throwL $ show node ++ " is not a function"

assertIsASTInteger :: AST -> LContext AST
assertIsASTInteger ast@(AST { astNode = node }) = case node of
    (ASTInteger _) -> return ast
    _ -> throwL $ show node ++ " is not an integer"

assertIsASTSymbol :: AST -> LContext AST
assertIsASTSymbol ast@(AST { astNode = node }) = case node of
    (ASTSymbol _) -> return ast
    _ -> throwL $ show node ++ " is not a symbol"

assertIsASTVector :: AST -> LContext AST
assertIsASTVector ast@(AST { astNode = node }) = case node of
    (ASTVector _) -> return ast
    _ -> throwL $ show node ++ " is not a vector"

assertIsASTString :: AST -> LContext AST
assertIsASTString ast@(AST { astNode = node }) = case node of
    (ASTString _) -> return ast
    _ -> throwL $ show node ++ " is not a string"

assertIsASTFunctionCall :: AST -> LContext AST
assertIsASTFunctionCall ast@(AST { astNode = node }) = case node of
    (ASTFunctionCall _) -> return ast
    _ -> throwL $ show node ++ " is not a function call or body"

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

makeNonsenseToken :: String -> Token
makeNonsenseToken content =
    Token { tokenContent = content, tokenRow = -1, tokenColumn = -1, tokenFileName = "nonsense" }

makeNonsenseAST :: ASTNode -> AST
makeNonsenseAST node =
    AST { astNode = node, astRow = -1, astColumn = -1, astFileName = "nonsense"}

pos :: AST -> String
pos AST { astRow = r, astColumn = c, astFileName = f } = f ++ ":" ++ show r ++ ":" ++ show c
