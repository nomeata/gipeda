module ParentMap where

import Data.Csv hiding (encode)
import qualified Data.Map as M
import Data.List.Split
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V
import Data.Char
import Data.Functor

type Hash = String
type ParentMap = M.Map Hash Hash

ssvFileToMap :: FilePath -> IO ParentMap
ssvFileToMap fname = ssvToMap <$> BS.readFile fname

ssvToMap :: BS.ByteString -> M.Map Hash Hash
ssvToMap s = M.fromList 
    [ (k,p)
    | (k,parentList) <- either error V.toList $ decodeWith ssv NoHeader s
    , let parents = splitOn " " parentList
    , p:_ <- return parents
    ]
    
ssv :: DecodeOptions
ssv = defaultDecodeOptions {
    decDelimiter = fromIntegral (ord ';')
}
