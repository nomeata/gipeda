{-# LANGUAGE CPP #-}
module Data.Text.Binary where

#if MIN_VERSION_text(1,2,1)
#else
-- I do not want a new dependency just for these, so this is copied from text-binary
instance Binary T.Text where
    put = put . T.encodeUtf8
    get = T.decodeUtf8 <$> get
#endif


