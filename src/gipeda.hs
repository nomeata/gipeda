import System.Environment
import System.IO
import System.Directory
import Control.Monad

import Shake
import JsonSettings
import RevReport
import WithLatestLogs
import Summary
import GraphReport
import BenchNames
import GraphSummaries

import GHC.IO.Encoding

{-
We want to build everything into one executable, bit still treat
it as multiple tools. Hence the main function
here calls the various real main functions, defaulting to the shake tool.
-}

main :: IO ()
main = do
 
    -- Make us locale-independent 
    setLocaleEncoding utf8

    args <- getArgs

    ex <- doesFileExist "gipeda.yaml"
    unless ex $ do
        hPutStr stderr "Please run this from the same directory as the gipeda.yaml file.\n"


    case args of 
        "JsonSettings":_      -> jsonSettingsMain
        "Summary":opts        -> summaryMain opts
        "RevReport":opts      -> revReportMain opts
        "BranchReport":opts   -> branchReportMain opts
        "GraphReport":opts    -> graphReportMain opts
        "WithLatestLogs":opts -> withLatestLogsMain opts
        "BenchNames":opts     -> benchNamesMain opts
        "GraphSummaries":opts  -> graphSummaries opts
        _ -> shakeMain -- shake will take the arguments from getArgs
