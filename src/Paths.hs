module Paths where

import System.FilePath

type Hash = String

out :: FilePath
out = "site" </> "out"

resultsOf, reportOf, summaryOf, logsOf, graphFile :: Hash -> FilePath

logsOf hash = "logs" </> hash <.> "log"
resultsOf hash = out </> "results" </> hash <.> "csv"
reportOf hash = out </> "reports" </> hash <.> "json"
summaryOf hash = out </> "summaries" </> hash <.> "json"
graphFile bench = out </> "graphs" </> bench <.> "json"
