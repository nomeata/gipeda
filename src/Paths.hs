module Paths where

import System.FilePath

type Hash = String

resultsOf, reportOf, summaryOf, logsOf, graphFile :: Hash -> FilePath

resultsOf hash = "results" </> hash <.> "csv"
reportOf hash = "reports" </> hash <.> "json"
summaryOf hash = "summaries" </> hash <.> "json"
logsOf hash = "logs" </> hash <.> "log"
graphFile bench = "graphs" </> bench <.> "json"
