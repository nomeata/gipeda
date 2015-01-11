module IndexReport where

import qualified Data.ByteString.Lazy as BS
import Data.Aeson
import Control.Monad

import Paths
import JsonUtils


indexReportsMain :: [FilePath] -> IO ()
indexReportsMain revs = do
    g <- forM revs $ \rev -> do
        json <- BS.readFile (summaryOf rev)
        case eitherDecode json of
            Left e -> fail e
            Right rep -> return (rep :: Value)

    BS.putStr (encode (merges g))
