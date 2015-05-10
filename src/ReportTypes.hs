{-# LANGUAGE DeriveGeneric, ViewPatterns, RecordWildCards, OverloadedStrings #-}

module ReportTypes where

import qualified Data.Map as M
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Text.Printf
import Data.List

import Paths
import ReadResult
import qualified BenchmarkSettings as S

data ClientSettings = ClientSettings
   { title :: String
   , cgitLink :: String
   , logLink :: String
   }
 deriving (Generic)
instance ToJSON ClientSettings

data GlobalReport = GlobalReport
    { settings :: Maybe ClientSettings
    , benchmarks :: Maybe (M.Map BenchName ())
    , revisions :: Maybe (M.Map Hash RevReport)
    , benchGroups :: Maybe [BenchGroup]
    }

instance ToJSON GlobalReport where
    toJSON (GlobalReport {..}) = object $
        ( "settings"    .=? settings ) ++
        ( "benchmarks"  .=? benchmarks ) ++
        ( "revisions"   .=? revisions ) ++
        ( "benchGroups" .=? benchGroups )
      where
        k .=? Just v  = [ k .= toJSON v ]
        _ .=? Nothing = []


emptyGlobalReport :: GlobalReport
emptyGlobalReport = GlobalReport Nothing Nothing Nothing Nothing


data SummaryStats = SummaryStats
    { totalCount :: Int
    , improvementCount :: Int
    , regressionCount :: Int
    , summaryDesc :: String
    }
 deriving (Show, Generic)
instance ToJSON SummaryStats
instance FromJSON SummaryStats

{-
sumStats :: [SummaryStats] -> SummaryStats
sumStats = foldl' go (SummaryStats 0 0 0)
  where go (SummaryStats a b c) (SummaryStats a' b' c') =
            SummaryStats (a + a') (b + b') (c + c')
-}

data Summary = Summary
    { hash :: Hash
    , gitDate :: Integer
    , gitSubject :: String
    , stats :: SummaryStats
    , parents :: [String]
    }
 deriving (Generic)
instance ToJSON Summary
instance FromJSON Summary


data RevReport = RevReport
    { summary :: Summary
    , gitLog :: String
    , benchResults :: M.Map BenchName BenchResult
    }
 deriving (Generic)
instance ToJSON RevReport
instance FromJSON RevReport

data ChangeType = Improvement | Boring | Regression
 deriving (Eq, Generic)
instance ToJSON ChangeType
instance FromJSON ChangeType

data BenchGroup = BenchGroup
    { groupName :: String
    , groupMembers :: [BenchName]
    }
 deriving (Generic)
instance ToJSON BenchGroup
instance FromJSON BenchGroup

data BenchResult = BenchResult
    { name :: String
    , value :: BenchValue
    , previous :: Maybe BenchValue
    , change :: String
    , changeType :: ChangeType
    , unit :: String
    , important :: Bool
    }
 deriving (Generic)
instance ToJSON BenchResult where
    toJSON = genericToJSON defaultOptions
instance FromJSON BenchResult

invertChangeType :: ChangeType -> ChangeType
invertChangeType Improvement = Regression
invertChangeType Boring = Boring
invertChangeType Regression = Improvement

type Explanation = (String, ChangeType)

noExplanation :: Explanation
noExplanation = ("", Boring)

equalExplanation :: Explanation
equalExplanation = ("=", Boring)

explainSmallInt :: S.BenchSettings -> Integer -> Integer -> Explanation
explainSmallInt _ i1 i2
    | i2 == i1 = equalExplanation 
    | i2 > i1 = ("+ " ++ show (i2 - i1), Improvement)
    | i2 < i1 = ("- " ++ show (i1 - i2), Regression)

explainInt :: S.BenchSettings -> Integer -> Integer -> Explanation
explainInt s i1 i2 = explainFloat s (fromIntegral i1) (fromIntegral i2)

explainFloat :: S.BenchSettings -> Double -> Double -> Explanation
explainFloat _ 0 0 = equalExplanation
explainFloat _ 0 _ = ("+ ∞", Improvement)
explainFloat s f1 f2 = (change, typ)
  where
    change | abs perc < 0.01 = "="
           | perc  >= 0 = printf "+ %.2f%%" perc
           | perc  <  0 = printf "- %.2f%%" (-perc)
    typ | abs perc < th = Boring
        | perc  >= 0 = Improvement
        | perc  <  0 = Regression

    perc = 100 * ((f2 - f1) / f1)
    th = S.threshold s

toFloat :: BenchValue -> Double
toFloat (I i) = fromIntegral i
toFloat (F f) = f

explain :: S.BenchSettings -> BenchValue -> BenchValue -> (String, ChangeType)
explain s@(S.numberType -> S.SmallIntegralNT) (I i1) (I i2) = explainSmallInt s i1 i2
explain s@(S.numberType -> S.IntegralNT)      (I i1) (I i2) = explainInt s i1 i2
explain s@(S.numberType -> S.FloatingNT)      v1     v2     = explainFloat s (toFloat v1) (toFloat v2)
explain _ _ _ = noExplanation

toResult :: S.BenchSettings -> String -> BenchValue -> Maybe BenchValue -> BenchResult
toResult s name value prev = BenchResult
    { name = name
    , value = value
    , previous = prev
    , change = change
    , changeType = changeType
    , unit = S.unit s
    , important = S.important s
    }
  where 
    (change, changeType') =
        case prev of
            Just p -> explain s p value
            Nothing -> noExplanation
    changeType | S.smallerIsBetter s = invertChangeType changeType'
               | otherwise           =                  changeType'

toSummaryStats :: [BenchResult] -> SummaryStats
toSummaryStats res = SummaryStats
    { totalCount = length res
    , improvementCount = length
        [ ()
        | BenchResult { changeType = Improvement, important = True } <- res
        ]
    , regressionCount =  length
        [ ()
        | BenchResult { changeType = Regression, important = True } <- res
        ]
    , summaryDesc = andMore 5
        [ name r ++ ": " ++ change r
        | r <- res, important r, changeType r `elem` [Improvement, Regression]
        ]
    }

andMore :: Int -> [String] -> String
andMore _ [] = "–"
andMore n xs = intercalate "\n" (take n xs) ++ rest
  where rest | length xs > n = "\nand " ++ show (length xs - n) ++ " more"
             | otherwise     = ""

{-
toGroup :: String -> [BenchResult] -> BenchGroup
toGroup n res = BenchGroup
    { groupName = n
    , benchResults = res
    , groupStats = SummaryStats
        { totalCount = length res
        , improvementCount = length [ () | BenchResult { changeType = Improvement } <- res ]
        , regressionCount =  length [ () | BenchResult { changeType = Regression } <- res ]
        }
    }
-}

createReport ::
    S.Settings -> Hash -> [Hash] ->
    ResultMap -> ResultMap ->
    String -> String -> Integer ->
    RevReport
createReport settings this parents thisM parentM log subject date = RevReport 
    { summary = Summary
        { hash = this
        , parents = parents
        , stats = toSummaryStats $ M.elems results
        , gitSubject = subject
        , gitDate = date
        }
    --, benchGroups = benchGroups
    , benchResults = results
    , gitLog = log
    }
  where
    results = M.fromList
        [ (name, toResult s name value (M.lookup name parentM))
        | (name, value) <- M.toAscList thisM
        , let s = S.benchSettings settings name
        ]

summarize :: RevReport -> Summary
summarize (RevReport {..}) =  summary 
