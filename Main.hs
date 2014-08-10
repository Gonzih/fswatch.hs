{-# LANGUAGE OverloadedStrings #-}

import System.FSNotify
import System.Process (system)
import System.Exit (ExitCode(..))
import Control.Monad (forever)
import Filesystem.Path.CurrentOS (encodeString)
import Data.List (isInfixOf)

mainFile :: String
mainFile = "./.fswatch"

eventPathString :: Event -> String
eventPathString event = encodeString $ eventPath event

eventType :: Event -> String
eventType (Added    _ _) = "added"
eventType (Modified _ _) = "modified"
eventType (Removed  _ _) = "removed"

q :: String -> String
q str = "\"" ++ str ++ "\""

handler :: Event -> IO ()
handler event = do
    let path  = eventPathString event
        eType = eventType event
        cmd   = unwords [ "sh"
                        , q mainFile
                        , q path
                        , q eType
                        ]
    code <- system cmd
    case code of
        ExitFailure exitCode -> putStr "Execution of helper failed. Exit code: " >> print exitCode
        _                    -> return ()

predicate :: Event -> Bool
predicate event = not $ "./.fswatch" `isInfixOf` eventPathString event

main :: IO ()
main = do
    putStrLn "Watching current directory for changes."
    withManager $ \mgr -> do
        _ <- watchTree
            mgr          -- manager
            "."          -- directory to watch
            predicate    -- predicate
            handler      -- action

        forever getLine

-- TODO:
-- parse console options
-- ignore patterns
-- -vv option
