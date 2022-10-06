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
        ("-r":rest) -> parseArgs
            config { configUseREPL = True } rest
        _ -> config

repl :: LState -> InputT IO ()
repl = repl' 1

repl' :: LineNo -> LState -> InputT IO ()
repl' lineNo ls = do
    minput <- getInputLine $ show lineNo ++ ":> "
    case minput of
        Nothing -> return ()
        Just input -> do
            result <- lift $ runL ls $ runInlineScript' lineNo "<repl>" input
            case result of
                Left (LException mp ex) -> do
                    outputStrLn $ case mp of
                        Just p -> p ++ " error: " ++ ex
                        Nothing -> "error: " ++ ex
                    repl' (lineNo + 1) ls
                Right (_, LState { stateEnv = newEnv }) -> do
                    repl' (lineNo + 1) ls { stateEnv = newEnv }

main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName

    let initialConfig = Config {
        configScriptFileName = Nothing,
        configVerboseMode = False,
        configShowHelp = False,
        configPrintEvaled = False,
        configPrintCallStack = False,
        configUseREPL = False
    }

    let config = parseArgs initialConfig args
        ls = LState { stateConfig = config, stateEnv = builtinEnv, stateDepth = 0 }

    if (configShowHelp config) then do
        putStrLn $ "Usage: " ++ progName ++ "                 # to open REPL"
        putStrLn $ "       " ++ progName ++ " -i scriptFile   # to run script file"
        putStrLn $ "       " ++ progName ++ " -h              # to show this help"
        putStrLn $ "       " ++ progName ++ " -e              # to automatically print results of evaluated expressions to stdout"
        putStrLn $ "       " ++ progName ++ " -s              # to automatically print call stack of evaluated expressions to stdout"
        putStrLn $ "       " ++ progName ++ " -r              # to open REPL, even after script file execution"
    else do
        when (configVerboseMode config) $ do
            putStrLn $ "configScriptFileName:\t" ++ (show $ configScriptFileName config)
            putStrLn $ "configVerboseMode:\t" ++ (show $ configVerboseMode config)
            putStrLn $ "configShowHelp:\t" ++ (show $ configShowHelp config)
            putStrLn $ "configPrintEvaled:\t" ++ (show $ configPrintEvaled config)
            putStrLn $ "configPrintCallStack:\t" ++ (show $ configPrintCallStack config)
            putStrLn $ "configUseREPL:\t" ++ (show $ configUseREPL config)

        case (configScriptFileName config) of
            Just scriptFileName -> do
                result <- runL ls (runScriptFile scriptFileName)
                case result of
                    Left (LException mp ex) -> case mp of
                        Just p -> putStrLn $ p ++ " error: " ++ ex
                        Nothing -> putStrLn $ "error: " ++ ex
                    Right (_, LState { stateEnv = evaledEnv }) -> do
                        when (configUseREPL config) $
                            runInputT defaultSettings $ repl ls { stateEnv = evaledEnv }
            Nothing -> do
                putStrLn $ "Lang REPL"
                putStrLn $ "Use CTRL+D to exit"
                runInputT defaultSettings $ repl ls
