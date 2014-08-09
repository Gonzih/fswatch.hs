{-# LANGUAGE OverloadedStrings #-}

import System.FSNotify
import System.Process (system)
import System.Exit (ExitCode(..))
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Filesystem.Path.CurrentOS (encodeString)
import Control.Concurrent.MVar

eventType :: Event -> String
eventType (Added _ _)    = "added"
eventType (Modified _ _) = "modified"
eventType (Removed _ _)  = "removed"

handler :: Event -> IO ()
handler event = do
    let path = encodeString $ eventPath event
        eType = eventType event
        cmd = "./.fswatch \"" ++ path  ++ "\" \"" ++ eType ++ "\""
    code <- system cmd
    case code of
      ExitFailure i -> putStr "Execution of helper failed. Exit code: " >> print i
      _             -> return ()

main :: IO ()
main = do
  putStrLn "Watching current directory for changes."
  withManager $ \mgr -> do
    _ <- watchTree
      mgr          -- manager
      "."          -- directory to watch
      (const True) -- predicate
      handler      -- action

    forever $ threadDelay maxBound

-- TODO:
-- add lock (maybe not)
-- parse console options
-- ignore patterns
