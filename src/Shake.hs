{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, NondecreasingIndentation #-}
module Shake where

import Prelude hiding ((*>))

import Development.Shake hiding (withTempFile)
import Development.Shake.FilePath
import Development.Shake.Classes
import Control.Monad
import qualified Data.Map as M
import Data.Functor
import Data.List
import System.IO.Extra (newTempFile)
import qualified Data.ByteString as BS
import qualified System.Directory

import Development.Shake.Gitlib

import Paths hiding (Hash)
import ParentMap
import BenchmarksInCSV
import qualified BenchmarkSettings as S

{- Global settings -}
cGRAPH_HISTORY :: Integer
cGRAPH_HISTORY = 50

git :: (CmdResult b) => String -> [String] -> Action b
git gitcmd args = do
    cmd (Traced $ "git " ++ gitcmd) (words "git -C repository" ++ gitcmd : args)

self :: (CmdResult b) => String -> [String] -> Action b
self name args = do
    -- orderOnly ["gipeda"]
    cmd (Traced name) "./gipeda" name args

gitRange :: Action String
gitRange = do
    s <- liftIO $ S.readSettings "settings.yaml"
    let first = S.start s
    [head] <- readFileLines "site/out/head.txt"
    return $ first ++ ".." ++ head

needIfThere :: [FilePath] -> Action [FilePath]
needIfThere files = do
    existing <- filterM doesFileExist files
    need existing
    return existing

doesLogExist :: LogSource -> Hash -> Action Bool
doesLogExist BareGit    hash = doesGitFileExist "logs" (hash <.> "log")
doesLogExist FileSystem hash = doesFileExist (logsOf hash)
doesLogExist NoLogs     hash = doesFileExist (resultsOf hash)

findPred, findPredOrSelf :: LogSource -> ParentMap -> Hash -> Action (Maybe Hash)
findPredOrSelf logSource m h = do
    ex <- doesLogExist logSource h
    if ex then return (Just h)
          else findPred logSource m h
findPred logSource m h = case M.lookup h m of
    Just h' -> findPredOrSelf logSource m h'
    Nothing -> return Nothing

findRecent :: LogSource -> ParentMap -> Integer -> FilePath -> Action [FilePath]
findRecent _ _ 0 _ = return []
findRecent logSource m n h = do
    pM <- findPred logSource m h
    (h:) <$> case pM of
        Nothing -> return []
        Just p ->  findRecent logSource m (n-1) p

newtype LimitRecent = LimitRecent ()
    deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

data LogSource = FileSystem | BareGit | NoLogs deriving Show

determineLogSource :: IO LogSource
determineLogSource = do
    haveLogs <- System.Directory.doesDirectoryExist "logs"
    if haveLogs
    then do
        Stdout s <- cmd "git -C logs rev-parse --is-bare-repository"
        if s == "true\n"
        then return BareGit
        else return FileSystem
    else return NoLogs

shakeMain :: IO ()
shakeMain = do
    logSource <- determineLogSource
    print logSource

    shakeArgs shakeOptions $ do
    defaultRuleGitLib

{-
    "gipeda" *> \out ->  do
        sources <- getDirectoryFiles "src" ["*.hs"]
        need (map ("src" </>) sources)
        cmd "ghc -isrc --make -O src/gipeda.hs -o" out
    want ["gipeda"]
-}

    getLimitRecent <- addOracle $ \(LimitRecent _) -> do
        need ["settings.yaml"]
        S.limitRecent <$> liftIO (S.readSettings "settings.yaml")

    "reports" ~> do
        range <- gitRange
        Stdout range <- git "log" ["--format=%H",range]
        let hashes = words range
        withLogs <- filterM (doesLogExist logSource) hashes
        need $ map reportOf withLogs
    want ["reports"]

    "summaries" ~> do
        range <- gitRange
        Stdout range <- git "log" ["--format=%H",range]
        let hashes = words range
        withLogs <- filterM (doesLogExist logSource) hashes
        need $ map summaryOf withLogs
    want ["summaries"]

    "site/out/head.txt" *> \ out -> do
        alwaysRerun
        Stdout stdout <- git "rev-parse" ["master"]
        writeFileChanged out stdout


    "site/out/history.csv" *> \out -> do
        range <- gitRange
        Stdout stdout <- git "log" ["--format=%H;%P",range]
        writeFileChanged out stdout
    want ["site/out/history.csv"]

    history' <- newCache $ \() -> do
         orderOnly ["site/out/history.csv"]
         liftIO $ ssvFileToMap "site/out/history.csv"
    let history = history' ()
    let pred h = do { hist <- history; findPred logSource hist h }
    let predOrSelf h = do { hist <- history; findPredOrSelf logSource hist h }
    let recent n h = do { hist <- history; findRecent logSource hist n h }

    "site/out/latest.txt" *> \ out -> do
        [head] <- readFileLines "site/out/head.txt"
        latestM <- predOrSelf head
        case latestM of
           Just latest ->
                writeFileChanged out latest
           Nothing ->
                fail "Head has no parent with logs?"

    "graphs" ~> do
        [latest] <- readFileLines "site/out/latest.txt"
        need [resultsOf latest]
        b <- liftIO $ benchmarksInCSVFile (resultsOf latest)
        need (map graphFile b)
    want ["graphs"]

    case logSource of
        BareGit ->
            "site/out/results/*.csv" *> \out -> do
                let hash = takeBaseName out
                withTempFile $ \fn -> do
                    log <- readGitFile "logs" (hash <.> "log")
                    liftIO $ BS.writeFile fn log
                    Stdout csv <- cmd "./log2csv" fn
                    writeFile' out csv
        FileSystem ->
            "site/out/results/*.csv" *> \out -> do
                let hash = takeBaseName out
                need [logsOf hash]
                Stdout csv <- cmd "./log2csv" (logsOf hash)
                writeFile' out csv
        NoLogs -> return ()

    "site/out/graphs//*.json" *> \out -> do
        let bench = dropDirectory1 (dropDirectory1 (dropDirectory1 (dropExtension out)))

        [latest] <- readFileLines "site/out/latest.txt"
        limitRecent <- getLimitRecent (LimitRecent ())
        r <- recent limitRecent latest
        need (map reportOf r)

        Stdout json <- self "GraphReport" (bench : r)
        writeFile' out json

    "site/out/reports/*.json" *> \out -> do
        let hash = takeBaseName out
        need [resultsOf hash]

        pred <- pred hash
        need [resultsOf h | Just h <- return pred]

        Stdout json <- self "RevReport" (hash : [h | Just h <- return pred])
        writeFile' out json

    "site/out/summaries/*.json" *> \out -> do
        let hash = takeBaseName out
        need [reportOf hash]

        Stdout json <- self "Summary" [hash]
        writeFile' out json

    "site/out/latest-summaries.json" *> \out -> do
        [latest] <- readFileLines "site/out/latest.txt"
        r <- recent cGRAPH_HISTORY latest
        need (map summaryOf r)

        Stdout json <- self "IndexReport" r
        writeFile' out json
    want ["site/out/latest-summaries.json"]

    "site/out/benchNames.json" *> \out -> do
        [latest] <- readFileLines "site/out/latest.txt"
        need [resultsOf latest]
        b <- liftIO $ benchmarksInCSVFile (resultsOf latest)

        need ["settings.yaml"]

        Stdout json <- self "BenchNames" (nub b)
        writeFile' out json
    want ["site/out/benchNames.json"]


    "site/out/all-summaries.json" *> \out -> do
        range <- gitRange
        Stdout range <- git "log" ["--format=%H",range]
        let hashes = words range
        withLogs <- filterM (doesLogExist logSource) hashes
        need (map summaryOf withLogs)

        Stdout json <- self "IndexReport" withLogs
        writeFile' out json
    want ["site/out/all-summaries.json"]

    "site/out/settings.json" *> \out -> do
        need ["settings.yaml"]

        Stdout json <- self "JsonSettings" []
        writeFile' out json
    want ["site/out/settings.json"]

    phony "clean" $ do
        removeFilesAfter "site/out" ["//*"]


-- | Create a temporary file in the temporary directory. The file will be deleted
--   after the action completes (provided the file is not still open).
--   The 'FilePath' will not have any file extension, will exist, and will be zero bytes long.
--   If you require a file with a specific name, use 'withTempDir'.
withTempFile :: (FilePath -> Action a) -> Action a
withTempFile act = do
    (file, del) <- liftIO newTempFile
    act file `actionFinally` del
