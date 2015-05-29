{-# LANGUAGE OverloadedStrings #-}
module GraphSummaries where

import qualified Data.ByteString.Lazy as BS
import Data.Aeson
import Data.Aeson.Types
import Control.Monad
import qualified Data.Text as T
import qualified Data.Map as M

import Paths
import JsonUtils
import ReportTypes
import ReadResult

graphSummaries :: [BenchName] -> IO ()
graphSummaries benchNames = do
    g <- forM benchNames $ \bName -> do
        json <- BS.readFile (graphFile bName)
        graph <- case eitherDecode json of
            Left e -> fail e
            Right rep -> return rep
        let gps = either (error.show) id . flip parseEither graph $ \obj -> do
                revs <- obj .: "revisions"
                forM (M.elems (revs :: M.Map T.Text Object)) $ \rev -> do
                    results <- rev .: "benchResults"
                    result <- results .: T.pack bName
                    parseJSON result
        return $ object [
            T.pack bName .= object
                [ T.pack "improvements" .= length [ () | gp <- gps, gpChangeType gp == Improvement ]
                , T.pack "regressions"  .= length [ () | gp <- gps, gpChangeType gp == Regression]
                ]
            ]
    let o = object [ "graphSummaries" .= merges g ]
    BS.putStr $ encode o
