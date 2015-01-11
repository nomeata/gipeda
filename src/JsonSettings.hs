{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module JsonSettings where

import Data.Aeson
import qualified Data.ByteString.Lazy as BS

import BenchmarkSettings as S

jsonSettingsMain :: IO ()
jsonSettingsMain = do
    Settings {..} <- S.readSettings "settings.yaml"
    let o = object
            [ "settings" .= object
                [ "title" .= title
                , "cgitLink" .= cgitLink
                , "logLink" .= logLink
                , "limitRecent" .= limitRecent
                ]
            ]
    BS.putStr (encode o)



    

