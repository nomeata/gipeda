{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module BenchmarkSettings where

import Data.Yaml
import Data.Aeson
import qualified Data.Vector as V
import Data.Functor
import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Maybe
import GHC.Generics (Generic)


data NumberType = IntegralNT | SmallIntegralNT | FloatingNT
    deriving (Show)

instance ToJSON NumberType where
    toJSON IntegralNT =      String "integral"
    toJSON SmallIntegralNT = String "small integral"
    toJSON FloatingNT =      String "floating"

type BenchName = String
data BenchSettings = BenchSettings
    { smallerIsBetter :: Bool
    , unit :: String
    , unitFull :: Maybe String
    , rescalable :: Bool
    , numberType :: NumberType
    , group :: String
    , threshold :: Double
    , important :: Bool
    }
    deriving (Show, Generic)
instance ToJSON BenchSettings

-- Keep in sync with setting_for() in gipeda.js
defaultBenchSettings :: BenchSettings
defaultBenchSettings = BenchSettings True "" Nothing False IntegralNT "" 3 True

newtype S = S { unS :: BenchName -> BenchSettings }
newtype SM = SM (BenchName -> (BenchSettings -> BenchSettings))

instance Semigroup SM where
    (<>) (SM f) (SM g) = SM (\n -> g n . f n)

instance Monoid SM where
    mempty = SM (const id)

instance FromJSON NumberType where
    parseJSON = withText "type" $ \t -> case t of
        "small integral" -> return SmallIntegralNT
        "integral" -> return IntegralNT
        "float" -> return FloatingNT
        v -> fail $ "Unknown \"type\": " ++ show v

-- Very naive glob, * only at the end
matches :: String -> String -> Bool
matches [] [] = True
matches _ ('*':[]) = True
matches (x:xs) (m:ms) = x == m && matches xs ms

instance FromJSON SM where
    parseJSON (Object o) = do
        m <- o .: "match"
        mt <- o .:? "type"
        mu <- o .:? "unit"
        muf <- o.:? "unitFull"
        mr <- o .:? "rescalable"
        mg <- o .:? "group"
        ms <- o .:? "smallerIsBetter"
        mth <- o .:? "threshold"
        mi <- o .:? "important"
        return $ SM $ \n b ->
            if n `matches` m then
               b { numberType      = fromMaybe (numberType b) mt
                 , unit            = fromMaybe (unit b) mu
                 , unitFull        = muf
                 , rescalable      = fromMaybe (rescalable b) mr
                 , group           = fromMaybe (group b) mg
                 , smallerIsBetter = fromMaybe (smallerIsBetter b) ms
                 , threshold       = fromMaybe (threshold b) mth
                 , important       = fromMaybe (important b) mi
                 }
            else b
    parseJSON _ = mzero

instance FromJSON S where
    parseJSON (Array a) = do
        mods <- mapM parseJSON (V.toList a)
        let SM sm = mconcat mods
        return $ S $ \n -> sm n defaultBenchSettings

data Settings = Settings
   { title :: String
   , revisionInfo :: String
   , diffLink :: Maybe String
   , limitRecent :: Integer
   , start :: Maybe String
   , interestingTags :: Maybe String
   , interestingBranches :: Maybe String
   , benchSettings :: BenchName -> BenchSettings
   }

instance FromJSON Settings where
    parseJSON (Object v) =
        Settings <$> v .: "title"
                 <*> v .: "revisionInfo"
                 <*> v .: "diffLink"
                 <*> v .: "limitRecent"
                 <*> v .:? "start"
                 <*> v .:? "interestingTags"
                 <*> v .:? "interestingBranches"
                 <*> (unS <$> v.: "benchmarks")
    parseJSON _ = mzero


readSettings :: FilePath -> IO Settings
readSettings fname = either (error.show) id <$> decodeFileEither fname
