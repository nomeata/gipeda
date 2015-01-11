module WithLatestLogs where

import Development.Shake.Command
import System.Directory
import Control.Monad

import Paths

git args = cmd (words "git -C repository" ++ args)

withLatestLogsMain :: [String] -> IO ()
withLatestLogsMain (n:args) = do
    Stdout out <- git ["log", "--first-parent", "--format=%H", "master", "-n", n]
    let revs = words out
    
    logs <- filterM (doesFileExist . logsOf) revs

    () <- cmd args (map logsOf logs)
    return ()
