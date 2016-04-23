{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module EmbeddedFiles where

import Data.FileEmbed
import qualified Data.ByteString.Char8 as BS
import Data.Monoid
import System.Directory

marker :: BS.ByteString
marker = "Remove this line to prevent gipeda from overriding this file"

indexHtmlFile :: BS.ByteString
indexHtmlFile =
    $(embedFile "site/index.html") <>
    "\n<!-- " <> marker <> "-->\n"

gipedaJSFile :: BS.ByteString
gipedaJSFile = $(embedFile "site/js/gipeda.js") <>
    "\n// " <> marker <> "\n"


installJSLibsScript :: BS.ByteString
installJSLibsScript = $(embedFile "install-jslibs.sh") <>
    "\n# " <> marker <> "\n"

isMarked :: BS.ByteString -> Bool
isMarked bs = marker `BS.isInfixOf` BS.drop n bs
  where n = BS.length bs - BS.length marker - 4

isMarkedFile :: FilePath -> IO Bool
isMarkedFile filepath = do
    ex <- doesFileExist filepath
    if ex then isMarked <$> BS.readFile filepath
          else return True

