module Paths where

import System.FilePath

type Hash = String
type BranchName = String

out :: FilePath
out = "site" </> "out"

resultsOf, reportOf, summaryOf, logsOf :: Hash -> FilePath
graphFile :: String -> FilePath
branchSummaryOf, branchMergebaseOf :: BranchName -> FilePath

logsOf hash = "logs" </> hash <.> "log"
resultsOf hash = out </> "results" </> hash <.> "csv"
reportOf hash = out </> "reports" </> hash <.> "json"
summaryOf hash = out </> "summaries" </> hash <.> "json"
graphFile bench = out </> "graphs" </> bench <.> "json"
branchSummaryOf branch = out </> "branches" </> branch <.> "json"
branchMergebaseOf branch = out </> "branches" </> branch <.> "mergebase"
