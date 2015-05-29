{-# LANGUAGE CPP #-}
module Data.Text.Binary where

#if MIN_VERSION_text(1,2,1)
#else

import Development.Shake.Classes

import Data.Text
import Data.Text.Encoding
import Data.Functor

-- I do not want a new dependency just for these, so this is copied from text-binary
instance Binary Text where
    put = put . encodeUtf8
    get = decodeUtf8 <$> get
#endif


