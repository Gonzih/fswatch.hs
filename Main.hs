{-# LANGUAGE OverloadedStrings #-}

import System.FSNotify
import System.Process (system)
import System.Exit (ExitCode(..))
import Control.Monad (forever, when, liftM)
import Filesystem.Path.CurrentOS (encodeString)
import Control.Concurrent.MVar
import Control.Concurrent (forkIO)

eventType :: Event -> String
eventType (Added    _ _) = "added"
eventType (Modified _ _) = "modified"
eventType (Removed  _ _) = "removed"

isUnLocked :: MVar a -> IO Bool
isUnLocked lock = isEmptyMVar lock

handler :: MVar Int -> Event -> IO ()
handler lock event = do
    predicateResult <- isUnLocked lock
    when predicateResult $ do
        let path  = encodeString $ eventPath event
            eType = eventType event
            cmd   = "sh ./.fswatch \"" ++ path  ++ "\" \"" ++ eType ++ "\""
        putMVar lock 1
        _ <- forkIO $ do
            code <- system cmd
            _ <- takeMVar lock
            case code of
                ExitFailure exitCode -> putStr "Execution of helper failed. Exit code: " >> print exitCode
                _                    -> return ()
        return ()

main :: IO ()
main = do
    putStrLn "Watching current directory for changes."
    lock <- newEmptyMVar
    withManager $ \mgr -> do
        _ <- watchTree
            mgr            -- manager
            "."            -- directory to watch
            (const True)   -- predicate
            (handler lock) -- action

        forever getLine

-- TODO:
-- parse console options
-- ignore patterns
