{-# LANGUAGE OverloadedStrings #-}

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

eventPathString :: Event -> String
eventPathString event = encodeString $ eventPath event

eventType :: Event -> String
eventType (Added    _ _) = "added"
eventType (Modified _ _) = "modified"
eventType (Removed  _ _) = "removed"

q :: String -> String
q string = "\"" ++ string ++ "\""

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
                        , q mainFile
                        , q path
                        , q eType
                        ]
    filePresent <- doesFileExist mainFile
    when filePresent $ execute cmd

predicate :: Event -> Bool
predicate event = not $ mainFile `isInfixOf` eventPathString event

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
