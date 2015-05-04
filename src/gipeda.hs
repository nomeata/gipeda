import System.Environment
import System.IO
import System.Directory
import Control.Monad

import Shake
import JsonSettings
import RevReport
import WithLatestLogs
import Summary
import IndexReport
import GraphReport
import BenchNames

{-
We want to build everything into one executable, bit still treat
it as multiple tools. Hence the main function
here calls the various real main functions, defaulting to the shake tool.
-}

main :: IO ()
main = do
    args <- getArgs

    ex <- doesFileExist "settings.yaml"
    unless ex $ do
        hPutStr stderr "Please run this from the same directory as the settings.yaml file.\n"


    case args of 
        "IndexReport":opts    -> indexReportsMain opts
        "JsonSettings":_      -> jsonSettingsMain
        "Summary":opts        -> summaryMain opts
        "RevReport":opts      -> revReportMain opts
        "GraphReport":opts    -> graphReportMain opts
        "WithLatestLogs":opts -> withLatestLogsMain opts
        "BenchNames":opts     -> benchNamesMain opts
        _ -> shakeMain -- shake will take the arguments from getArgs
