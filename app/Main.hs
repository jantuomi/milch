module Main (main) where
import System.Environment
import Control.Monad.Except
import System.Console.Haskeline
import Utils
import Builtins
import Interpreter

parseArgs :: Config -> [String] -> Config
parseArgs config args =
    case args of
        ("-i":fileName:rest) -> parseArgs
            config { configScriptFileName = Just fileName } rest
        ("-v":rest) -> parseArgs
            config { configVerboseMode = True } rest
        ("-h":rest) -> parseArgs
            config { configShowHelp = True } rest
        ("-e":rest) -> parseArgs
            config { configPrintEvaled = True } rest
        ("-s":rest) -> parseArgs
            config { configPrintCallStack = True } rest
        _ -> config

repl :: Config -> Env -> InputT IO ()
repl config env = do
    minput <- getInputLine "> "
    case minput of
        Nothing -> return ()
        Just input -> do
            result <- lift $ runL config (runInlineScript "<repl>" env input)
            case result of
                Left (LException mp ex) -> do
                    outputStrLn $ case mp of
                        Just p -> p ++ " error: " ++ ex
                        Nothing -> "error: " ++ ex
                    repl config env
                Right (newEnv, _) -> do
                    repl config newEnv

main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName

    let initialConfig = Config {
        configScriptFileName = Nothing,
        configVerboseMode = False,
        configShowHelp = False,
        configPrintEvaled = False,
        configPrintCallStack = False
    }

    let config = parseArgs initialConfig args

    if (configShowHelp config) then do
        putStrLn $ "Usage: " ++ progName ++ "                 # to open REPL"
        putStrLn $ "       " ++ progName ++ " -i scriptFile   # to run script file"
        putStrLn $ "       " ++ progName ++ " -h              # to show this help"
        putStrLn $ "       " ++ progName ++ " -e              # to automatically print results of evaluated expressions to stdout"
        putStrLn $ "       " ++ progName ++ " -s              # to automatically print call stack of evaluated expressions to stdout"
    else do
        when (configVerboseMode config) $ do
            putStrLn $ "configScriptFileName:\t" ++ (show $ configScriptFileName config)
            putStrLn $ "configVerboseMode:\t" ++ (show $ configVerboseMode config)

        case (configScriptFileName config) of
            Just scriptFileName -> do
                result <- runL config (runScriptFile builtinEnv scriptFileName)
                case result of
                    Left (LException mp ex) -> case mp of
                        Just p -> putStrLn $ p ++ " error: " ++ ex
                        Nothing -> putStrLn $ "error: " ++ ex
                    Right _ -> return ()
            Nothing -> do
                putStrLn $ "Lang REPL"
                putStrLn $ "Use CTRL+D to exit"
                runInputT defaultSettings (repl config builtinEnv)

    return ()
