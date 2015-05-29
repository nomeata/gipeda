{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module GraphReport where

import Control.Monad
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import Data.Aeson
import GHC.Generics
import Data.Maybe
import Data.Functor

import Paths
import ReadResult
import ReportTypes
import qualified BenchmarkSettings as S

graphReportMain :: [String] -> IO ()
graphReportMain (bench:revs) = do
    settings <- S.readSettings "settings.yaml"

    g <- forM revs $ \rev -> do
        json <- BS.readFile (reportOf rev)
        rep <- case eitherDecode json of
            Left e -> fail e
            Right rep -> return rep
        case M.lookup bench (benchResults (rep !!! "revisions" !!! rev)) of
            Nothing -> return Nothing
            Just result -> return $ Just $ T.pack rev .= object
                        [ "benchResults" .= object
                            [ T.pack bench .= benchResultToGraphPoint result ]
                        ]
    let doc = object
                [ "revisions" .= object (catMaybes g)
                , "benchmarkSettings" .= object
                    [ T.pack bench .= toJSON (S.benchSettings settings bench) ]
                ]

    BS.putStr (encode doc)
  where
   m !!! k =  m M.! T.pack k
