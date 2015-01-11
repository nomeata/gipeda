{-# LANGUAGE DeriveGeneric #-}

module ReadResult where

import Data.Csv hiding (encode)
import Data.Functor
import Control.Applicative
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V
import System.Directory
import Data.Aeson hiding (decode)
import GHC.Generics
import Data.Char

import Paths

type BenchName = String

data BenchValue =
    F Double |
    I Integer
 deriving (Show, Generic, Eq, Ord)

type ResultMap = M.Map String BenchValue

instance ToJSON BenchValue where 
    toJSON (F d) = toJSON d
    toJSON (I i) = toJSON i
instance FromJSON BenchValue where 
    parseJSON o = (I <$> parseJSON o) <|> (F <$> parseJSON o)

instance FromField BenchValue where
    parseField s = (I <$> parseField s) <|> (F <$> parseField s)

readCSV :: Hash -> IO ResultMap
readCSV hash = do 
    ex <- doesFileExist (resultsOf hash)
    if not ex then return M.empty else do
    str <- BS.readFile (resultsOf hash)
    let rows = either error id $ decodeWith ssv NoHeader str
    return $ M.fromList $ V.toList $ rows

ssv :: DecodeOptions
ssv = defaultDecodeOptions {
    decDelimiter = fromIntegral (ord ';')
}

