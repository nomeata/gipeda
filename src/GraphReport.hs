{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module GraphReport where

import Control.Monad
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import Data.Aeson
import GHC.Generics

import Paths
import ReadResult
import ReportTypes
import qualified BenchmarkSettings as S

graphReportMain :: [String] -> IO ()
graphReportMain (bench:revs) = do
    settings <- S.readSettings "settings.yaml"

    g <- forM revs $ \rev -> do
        m <- readCSV rev
        let v = M.lookup bench m
        return $ T.pack rev .= object
            [ "benchResults" .= object
                [ T.pack bench .= object
                    [ "value" .= v ]
                ]
            ]
    let doc = object
                [ "revisions" .= object g
                , "benchmarkSettings" .= object
                    [ T.pack bench .= toJSON (S.benchSettings settings bench) ]
                ]

    BS.putStr (encode doc)
    
