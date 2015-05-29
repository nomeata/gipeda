{-# LANGUAGE DeriveGeneric #-}

module RevReport where

import Data.Functor
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BS
import Data.Aeson
import Development.Shake.Command

import ReportTypes
import ReadResult
import qualified BenchmarkSettings as S

git args = cmd (words "git -C repository" ++ args)

revReportMain :: [String] -> IO ()
revReportMain (this:parents) = do
    settings <- S.readSettings "settings.yaml"

    thisM <- readCSV this
    parentM <- case parents of
        p:_ -> readCSV p
        _    -> return M.empty

    log <- case parents of
        p:_ -> fromStdout <$> git ["log", p ++ ".."++ this]
        _   -> fromStdout <$> git ["show", "-s", this]

    msg <- fromStdout <$> git ["show", "--format=%s", "-s", this]
    date <- read . fromStdout <$> git ["show", "--format=%ct","-s",this]

    let rep = createReport settings this parents thisM parentM log msg date
    let doc = emptyGlobalReport { revisions = Just (M.singleton this rep) }

    BS.putStr (encode doc)
    
