{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.FSNotify
import System.Process (system)
import System.Exit (ExitCode(..))
import Control.Monad (forever, when)
import Filesystem.Path.CurrentOS (encodeString)
import Data.List (isInfixOf)
import System.Directory (doesFileExist)
import Options.Applicative

data CommandArgs = CommandArgs
    { executeWith :: String }

mainFile :: String
mainFile = ".fswatch"

charactersNotToEscape :: String
charactersNotToEscape = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-.,:/@\n"

charEscape :: Char -> String
charEscape c = if c `elem` charactersNotToEscape
                 then s
                 else '\\':s
               where s = [c]

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

predicate :: Event -> Bool
predicate = not . (mainFile `isInfixOf`) . eventPathString

watch :: CommandArgs -> IO ()
watch commandArgs = do
    putStrLn "Watching current directory for changes."
    withManager $ \mgr -> do
        _ <- watchTree
            mgr
            "."
            predicate
            (handler commandArgs)

        forever getLine

main :: IO ()
main = execParser opts >>= watch
    where opts = info (helper <*> argumentsParser)
            ( fullDesc
           <> progDesc "Watch current directory for changes."
           <> header   "fswatch - 0.1.0.0")

-- TODO:
-- ignore patterns
