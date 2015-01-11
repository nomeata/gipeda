{-# LANGUAGE RecordWildCards, DeriveGeneric, OverloadedStrings #-}
module Summary where

import qualified Data.ByteString.Lazy as BS
import Data.Aeson

import Paths
import JsonUtils

summaryMain :: [FilePath] -> IO ()
summaryMain [rev] = do
    json <- BS.readFile (reportOf rev)
    rep <- case eitherDecode json of
        Left e -> fail e
        Right rep -> return rep
    let rep' = delete ["revisions","*","benchResults"] $
               delete ["revisions","*","gitLog"] $
               rep
    BS.putStr (encode rep')
