{-# LANGUAGE OverloadedStrings #-}
module BenchNames where

import qualified Data.ByteString.Lazy as BS
import Data.Aeson
import Control.Monad
import qualified Data.Map as M
import qualified Data.Text as T
import Data.List

import Paths
import JsonUtils
import ReportTypes
import qualified BenchmarkSettings as S


benchNamesMain :: [S.BenchName] -> IO ()
benchNamesMain benchNames = do
    settings <- S.readSettings "gipeda.yaml"

    let groups =
            map (uncurry BenchGroup) $
            sort $
            M.toList $
            M.map sort $
            M.fromListWith (++)
                [ (S.group s, [bn])
                | bn <- benchNames
                , let s = S.benchSettings settings bn
                ]
    let doc = emptyGlobalReport { benchGroups = Just groups }

    BS.putStr (encode doc)
