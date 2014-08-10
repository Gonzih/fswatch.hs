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

predicate :: MVar a -> Event -> IO Bool
predicate lock _ = and `liftM` sequence [isEmptyMVar lock]

handler :: MVar Int -> Event -> IO ()
handler lock event = do
    predicateResult <- predicate lock event
    when predicateResult $ do
        let path  = encodeString $ eventPath event
            eType = eventType event
            cmd   = "./.fswatch \"" ++ path  ++ "\" \"" ++ eType ++ "\""
        putMVar lock 1
        _ <- forkIO $ do
            code <- system cmd
            _ <- takeMVar lock
            case code of
                ExitFailure i -> putStr "Execution of helper failed. Exit code: " >> print i
                _             -> return ()
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
