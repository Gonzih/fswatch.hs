{-# LANGUAGE OverloadedStrings #-}

module Main where

import Paths_fswatch (version)
import Control.Monad (forever, when)
import Data.Version (showVersion)
import Data.String (fromString)
import Data.List (isInfixOf)
import Filesystem.Path.CurrentOS (encodeString, (</>))
import System.Process (system)
import System.Exit (ExitCode(..))
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FSNotify
import Options.Applicative

data CommandArgs = CommandArgs
    { executeWith :: String }

mainFile :: String
mainFile = ".fswatch"

charactersNotToEscape :: String
charactersNotToEscape = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-.,:/@\n"

charEscape :: Char -> String
charEscape c | c `elem` charactersNotToEscape = [c]
             | otherwise                      = ['\\', c]

shellEscape :: String -> String
shellEscape = concatMap charEscape

eventPathString :: Event -> String
eventPathString = encodeString . eventPath

eventType :: Event -> String
eventType (Added    _ _) = "added"
eventType (Modified _ _) = "modified"
eventType (Removed  _ _) = "removed"

execute :: String -> IO ()
execute cmd = do
    code <- system cmd
    case code of
      ExitFailure exitCode -> putStr "Execution of helper failed. Exit code: " >> print exitCode
      _                    -> return ()

argumentsParser :: Parser CommandArgs
argumentsParser = CommandArgs
    <$> strOption
        ( long "execute-with"
       <> short 'e'
       <> value "sh"
       <> showDefault
       <> help "Executable to call file with")


handler :: CommandArgs -> Event -> IO ()
handler (CommandArgs shellExecutable) event = do
    let path  = eventPathString event
        eType = eventType event
        cmd   = unwords [ shellExecutable
                        , shellEscape mainFile
                        , shellEscape path
                        , shellEscape eType
                        ]
    filePresent <- doesFileExist mainFile
    when filePresent $ execute cmd

predicate :: String -> Event -> Bool
predicate cwd event = (fromString cwd </> fromString mainFile) /= eventPath event

watch :: CommandArgs -> IO ()
watch commandArgs = do
    putStrLn "Watching current directory for changes."
    cwd <- getCurrentDirectory
    withManager $ \mgr -> do
        _ <- watchTree
            mgr
            "."
            (predicate cwd)
            (handler commandArgs)

        forever getLine

main :: IO ()
main = execParser opts >>= watch
    where opts = info (helper <*> argumentsParser)
            ( fullDesc
           <> progDesc "Watch current directory for changes."
           <> header   ("fswatch - " ++ showVersion version))

-- TODO:
-- ignore patterns
