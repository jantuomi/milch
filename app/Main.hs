module Main (main) where
import System.Environment
import Control.Monad.Except
import Control.Monad.Reader
import System.Console.Haskeline
import Types
import Lib

parseArgs :: Config -> [String] -> Config
parseArgs config args =
    case args of
        ("-i":fileName:rest) -> parseArgs
            config { configScriptFileName = Just fileName } rest
        ("-v":rest) -> parseArgs
            config { configVerboseMode = True } rest
        ("-h":rest) -> parseArgs
            config { configShowHelp = True } rest
        _ -> config

repl :: Config -> InputT IO ()
repl config = do
    minput <- getInputLine "> "
    case minput of
        Nothing -> return ()
        Just input -> do
            result <- lift $ runExceptT $ runReaderT (runInlineScript input) config
            case result of
                Left (LException ex) -> outputStrLn $ "Error: " ++ ex
                Right () -> return ()
            repl config

main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName

    let initialConfig = Config {
        configScriptFileName = Nothing,
        configVerboseMode = False,
        configShowHelp = False
    }

    let config = parseArgs initialConfig args

    if (configShowHelp config) then do
        putStrLn $ "Usage: " ++ progName ++ "                 # to open REPL"
        putStrLn $ "       " ++ progName ++ " -i scriptFile   # to run script file"
        putStrLn $ "       " ++ progName ++ " -h              # to show this help"
    else do
        when (configVerboseMode config) $ do
            putStrLn $ "configScriptFileName:\t" ++ (show $ configScriptFileName config)
            putStrLn $ "configVerboseMode:\t" ++ (show $ configVerboseMode config)

        case (configScriptFileName config) of
            Just scriptFileName -> do
                result <- runExceptT $ runReaderT (runScriptFile scriptFileName) config
                case result of
                    Left (LException ex) -> putStrLn $ "Error: " ++ ex
                    Right () -> return ()
            Nothing -> do
                putStrLn $ "Lang REPL"
                putStrLn $ "Use CTRL+D to exit"
                runInputT defaultSettings (repl config)
    return ()
