{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module JsonSettings where

import Data.Yaml
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.Functor

jsonSettingsMain :: IO ()
jsonSettingsMain = do
    s <- either (error.show) id <$> decodeFileEither "gipeda.yaml"
    let o = object [ "settings" .= (s :: Object) ]
    BS.putStr (Data.Aeson.encode o)
