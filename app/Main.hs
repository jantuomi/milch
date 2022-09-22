module Main (main) where
import System.Environment
import Control.Monad

import Lib

data Config = Config {
    configScriptFileName :: Maybe String,
    configVerboseMode :: Bool,
    configShowHelp :: Bool
}

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

main :: IO ()
main = do
    args <- getArgs
    let initialConfig = Config {
        configScriptFileName = Nothing,
        configVerboseMode = False,
        configShowHelp = False
    }

    let config = parseArgs initialConfig args

    when (configVerboseMode config) $ do
        putStrLn $ "configScriptFileName:\t" ++ (show $ configScriptFileName config)
        putStrLn $ "configVerboseMode:\t" ++ (show $ configVerboseMode config)

    when (configShowHelp config) $ do
        error "TODO help"

    case (configScriptFileName config) of
        Just scriptFileName -> runScriptFile scriptFileName
        Nothing -> error "TODO REPL"

    return ()
