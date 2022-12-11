{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Utils where

import Control.Monad.Except
import Control.Exception (IOException, catch)
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Char as C
import qualified Data.Text as T
import qualified FarmHash as FH
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Maybe as MB
import qualified Data.Word as W

-- TYPES

type PositionString = String
type ErrorString = String
type StackRow = (PositionString, ErrorString)

data LException = LException [StackRow]

data PrintEvaled
    = PrintEvaledOff
    | PrintEvaledAll
    | PrintEvaledNonUnit
    deriving Show

data Config = Config {
    configScriptFileName :: Maybe String,
    configVerboseMode :: Bool,
    configShowHelp :: Bool,
    configPrintEvaled :: PrintEvaled,
    configPrintCallStack :: Bool,
    configUseREPL :: Bool
}
data Binding a
    = Regular a
    | Memoized (M.Map [a] a) a
    deriving Show

type SourceModule = String
type Env = M.Map String (SourceModule, Binding AST)
type Scope = [(String, AST)]
type AtomMap = M.Map LAtomRef AST

data LState = LState {
    stateConfig :: Config,
    stateEnv :: Env,
    stateDepth :: Int,
    statePure :: Purity,
    stateAtomMap :: AtomMap
}

type LineNo = Int

type LContext a = StateT LState (ExceptT LException IO) a

runL :: LState -> LContext a -> IO (Either LException (a, LState))
runL s lc = runExceptT $ (flip runStateT) s lc

getEnv :: LContext Env
getEnv = do
    s <- get
    return $ stateEnv s

getConfig :: LContext Config
getConfig = do
    s <- get
    return $ stateConfig s

getCurrentModule :: LContext SourceModule
getCurrentModule = do
    config <- getConfig
    let curMod = configScriptFileName config $> MB.fromMaybe "<repl>"
    return curMod

putEnv :: Env -> LContext ()
putEnv env = do
    modify (\s -> s { stateEnv = env })

insertEnv :: String -> Binding AST -> LContext ()
insertEnv k v = do
    env <- getEnv
    currentModule <- getCurrentModule
    putEnv $ M.insert k (currentModule, v) env

incrementDepth :: LContext ()
incrementDepth =
    modify (\s -> s { stateDepth = stateDepth s + 1 })

decrementDepth :: LContext ()
decrementDepth =
    modify (\s -> s { stateDepth = stateDepth s - 1 })

getDepth :: LContext Int
getDepth = do
    s <- get
    return $ stateDepth s

getPurity :: LContext Purity
getPurity = do
    s <- get
    return $ statePure s

getAtomMap :: LContext AtomMap
getAtomMap = do
    s <- get
    return $ stateAtomMap s

putAtomMap :: AtomMap -> LContext ()
putAtomMap atomMap = do
    modify (\s -> s { stateAtomMap = atomMap })

insertAtomMap :: LAtomRef -> AST -> LContext ()
insertAtomMap k v = do
    atomMap <- getAtomMap
    putAtomMap $ M.insert k v atomMap

createAtom :: AST -> LContext AST
createAtom ast = do
    atomMap <- getAtomMap
    let (LAtomRef maxKey) = if (M.size atomMap > 0)
        then fst $ M.findMax atomMap
        else LAtomRef 0
    let nextKey = maxKey + 1
    let ref = LAtomRef nextKey
    insertAtomMap ref ast
    return $ ast { an = ASTAtom ref }

isAllowedPurity :: Purity -> LContext Bool
isAllowedPurity purity = do
    s <- get
    let currentPurity = statePure s
    return $ case currentPurity of
        Impure -> True -- if currently in impure context (false), all calls are ok
        Pure -> purity == Pure -- but if in pure context (true), only pure calls are ok

updatePurity :: Purity -> LContext ()
updatePurity purity = do
    modify (\s -> s { statePure = purity })

checkPurity :: Purity -> LContext ()
checkPurity purity = do
    purityOk <- isAllowedPurity purity
    when (not purityOk) $ throwL ("", "cannot call impure function in pure context")

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

data Purity = Pure | Impure deriving (Eq, Show)
type LFunction = AST -> LContext AST

type LRecord = M.Map String AST
type TagHash = Int

newtype LAtomRef = LAtomRef Int deriving (Eq, Ord, Show)

data ASTNode
    = ASTInteger Integer
    | ASTDouble Double
    | ASTSymbol String
    | ASTBoolean Bool
    | ASTChar Char
    | ASTString String
    | ASTTag TagHash String
    | ASTVector [AST]
    | ASTFunctionCall [AST]
    | ASTHashMap (M.Map AST AST)
    | ASTFunction Purity LFunction
    | ASTRecord TagHash String LRecord
    | ASTAtom LAtomRef
    | ASTUnit
    | ASTHole

data AST = AST {
    an :: ASTNode,
    astRow :: Int,
    astColumn :: Int,
    astFileName :: String
}

instance (Show AST) where
    show (AST { an = node }) = show node

instance (Show ASTNode) where
    show (ASTInteger n) = show n
    show (ASTDouble n) = show n
    show (ASTSymbol s) = s
    show (ASTBoolean b) = show b $> map C.toLower
    show (ASTChar c) = show c
    show (ASTString s) = show s
    show (ASTTag _ s) = ":" ++ s
    show (ASTVector v) = "[" ++ L.intercalate " " (map show v) ++ "]"
    show (ASTFunctionCall v) = "(" ++ L.intercalate " " (map show v) ++ ")"
    show (ASTHashMap m) =
        let flattenMap = M.assocs .> L.concatMap (\(k, v) -> [k, v])
         in "{" ++ L.intercalate " " (map show $ flattenMap m) ++ "}"
    show (ASTFunction isPure _) = case isPure of
        Pure -> "<pure fn>"
        Impure -> "<impure fn>"
    show (ASTRecord _ identifier record) =
        let assocsStrList = map (\(k, v) -> k ++ ":" ++ show v) (M.assocs record)
         in "(" ++ identifier ++ " " ++ L.intercalate " " assocsStrList ++ ")"
    show (ASTAtom _) = "<atom>"
    show ASTUnit = "<unit>"
    show ASTHole = "<hole>"

instance (Eq AST) where
    AST { an = node1 } == AST { an = node2 } = node1 == node2

instance (Eq ASTNode) where
    ASTInteger a == ASTInteger b = a == b
    ASTDouble a == ASTDouble b = a == b
    ASTSymbol a == ASTSymbol b = a == b
    ASTBoolean a == ASTBoolean b = a == b
    ASTChar a == ASTChar b = a == b
    ASTString a == ASTString b = a == b
    ASTTag n _ == ASTTag m _ = n == m
    ASTVector a == ASTVector b = a == b
    ASTFunctionCall a == ASTFunctionCall b = a == b
    ASTHashMap a == ASTHashMap b = a == b
    ASTRecord ah _ hma == ASTRecord bh _ hmb = ah == bh && hma == hmb
    ASTAtom a == ASTAtom b = a == b
    ASTUnit == ASTUnit = True
    ASTHole == _ = True
    _ == ASTHole = True
    _ == _ = False

-- Ord instance needed for M.Map and lt? builtin
instance (Ord AST) where
    AST { an = node1 } <= AST { an = node2 } = node1 <= node2

instance (Ord ASTNode) where
    ASTInteger a <= ASTInteger b = a <= b
    ASTDouble a <= ASTDouble b = a <= b
    ASTSymbol a <= ASTSymbol b = a <= b
    ASTBoolean a <= ASTBoolean b = a <= b
    ASTChar a <= ASTChar b = a <= b
    ASTString a <= ASTString b = a <= b
    ASTTag n _ <= ASTTag m _ = n <= m
    ASTVector a <= ASTVector b = a <= b
    ASTFunctionCall a <= ASTFunctionCall b = a <= b
    ASTHashMap a <= ASTHashMap b = a <= b
    ASTRecord ah _ hma <= ASTRecord bh _ hmb = ah <= bh && hma <= hmb
    ASTAtom a <= ASTAtom b = a <= b
    ASTUnit <= ASTUnit = True
    ASTHole <= _ = True
    _ <= ASTHole = True
    _ <= _ = False

computeTagNSeed :: W.Word64
computeTagNSeed = 123

computeTagN :: String -> Int
computeTagN s =
    let hash = FH.hash64WithSeed (BSU.fromString s) computeTagNSeed
     in fromIntegral hash

assertIsASTFunction :: AST -> LContext AST
assertIsASTFunction ast@(AST { an = node }) = case node of
    (ASTFunction _ _) -> return ast
    _ -> throwL (astPos ast, show node ++ " is not a function")

assertIsASTInteger :: AST -> LContext AST
assertIsASTInteger ast@(AST { an = node }) = case node of
    (ASTInteger _) -> return ast
    _ -> throwL (astPos ast, show node ++ " is not an integer")

assertIsASTSymbol :: AST -> LContext AST
assertIsASTSymbol ast@(AST { an = node }) = case node of
    (ASTSymbol _) -> return ast
    _ -> throwL (astPos ast, show node ++ " is not a symbol")

assertIsASTVector :: AST -> LContext AST
assertIsASTVector ast@(AST { an = node }) = case node of
    (ASTVector _) -> return ast
    _ -> throwL (astPos ast, show node ++ " is not a vector")

assertIsASTString :: AST -> LContext AST
assertIsASTString ast@(AST { an = node }) = case node of
    (ASTString _) -> return ast
    _ -> throwL (astPos ast, show node ++ " is not a string")

assertIsASTFunctionCall :: AST -> LContext AST
assertIsASTFunctionCall ast@(AST { an = node }) = case node of
    (ASTFunctionCall _) -> return ast
    _ -> throwL (astPos ast, show node ++ " is not a function call or body")

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

throwL :: StackRow -> LContext a
throwL sr = throwError $ LException [sr]

appendError :: StackRow -> LException -> LContext a
appendError as (LException stack) =
    throwError $ LException $ as : stack

asPairsM :: [a] -> LContext [(a, a)]
asPairsM [] = return []
asPairsM (a:b:rest) = do
    restPaired <- asPairsM rest
    return $ (a, b) : restPaired
asPairsM _ = throwL ("", "odd number of elements to pair up")

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
    AST { an = node, astRow = -1, astColumn = -1, astFileName = "nonsense"}

astPos :: AST -> String
astPos AST { astRow = r, astColumn = c, astFileName = f } = f ++ ":" ++ show r ++ ":" ++ show c

tokenPos :: Token -> String
tokenPos Token { tokenRow = r, tokenColumn = c, tokenFileName = f } = f ++ ":" ++ show r ++ ":" ++ show c

separateNsIdPart :: String -> (String, String)
separateNsIdPart identifier =
    let t = T.pack identifier
        parts = T.splitOn (T.pack "/") t
        nsPartText = T.concat $ L.init parts
        idPartText = L.last parts
     in (T.unpack nsPartText, T.unpack idPartText)

safeReadFile :: FilePath -> IO (Maybe String)
safeReadFile p = (Just <$> readFile p) `catch` handler where
    handler :: IOException -> IO (Maybe String)
    handler _ = pure Nothing

safeWriteFile :: FilePath -> String -> IO (Maybe ())
safeWriteFile p content = (Just <$> writeFile p content) `catch` handler where
    handler :: IOException -> IO (Maybe ())
    handler _ = pure Nothing

safeAppendFile :: FilePath -> String -> IO (Maybe ())
safeAppendFile p content = (Just <$> appendFile p content) `catch` handler where
    handler :: IOException -> IO (Maybe ())
    handler _ = pure Nothing

foldStackMessage :: LException -> String
foldStackMessage (LException st) = case st of
    [] -> ""
    [(tp, ts)] -> "error: " ++ ts ++ (fmtPos tp)
    _ -> let revStack = reverse st
             (tp, ts) = head revStack
             restStack = tail revStack
             folded = L.foldr (\(p, s) acc -> acc ++ s ++ (fmtPos p) ++ ",\n") "" $ restStack
          in folded ++ "\n" ++ "error: " ++ ts ++ (fmtPos tp)
    where
        fmtPos p = if useless p then "" else (" at " ++ p)
        useless p = "nonsense" `L.isPrefixOf` p || length p == 0

fold1M :: (Monad m) => (a -> a -> m a) -> [a] -> m a
fold1M _ (x:[])         =  return x
fold1M f (x:y:xs)       =  do ret <- f x y
                              fold1M f (ret : xs)
fold1M _ []             =  error $ "fold1M of empty list"
