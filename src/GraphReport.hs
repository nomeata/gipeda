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
    settings <- S.readSettings "gipeda.yaml"
    let s = S.benchSettings settings bench

    g <- forM (withPrev revs) $ \(rev, prevM) -> do
        report <- readReport rev

        case M.lookup bench (benchResults report) of
            Nothing -> return Nothing
            Just result -> do
                changeType <- case prevM of
                    Nothing -> return Boring
                    Just prev -> do
                        prevReport <- readReport prev
                        case M.lookup bench (benchResults prevReport) of
                            Nothing -> return Boring
                            Just prevResult ->
                                let comp = makeComparison s bench (value result) (Just (value prevResult))
                                in return $ changeType comp

                return $ Just $ T.pack rev .= object
                        [ "benchResults" .= object
                            [ T.pack bench .= GraphPoint
                                { gpValue = value result
                                , gpChangeType = changeType
                                }
                            ]
                        ]
    let doc = object
                [ "revisions" .= object (catMaybes g)
                , "benchmarkSettings" .= object
                    [ T.pack bench .= toJSON s ]
                ]

    BS.putStr (encode doc)

readReport :: Hash -> IO RevReport
readReport hash = do
    json <- BS.readFile (reportOf hash)
    case eitherDecode json of
        Left e    -> fail e
        Right rep -> return $ rep !!! "revisions" !!! hash

(!!!) :: M.Map T.Text a -> String -> a
m !!! k =  m M.! T.pack k

withPrev :: [a] -> [(a, Maybe a)]
withPrev (x:y:ys) = (x, Just y) : withPrev (y:ys)
withPrev [x]      = [(x, Nothing)]
withPrev []       = []
