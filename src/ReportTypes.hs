{-# LANGUAGE DeriveGeneric, ViewPatterns, RecordWildCards, OverloadedStrings, CPP #-}

module ReportTypes where

import qualified Data.Map as M
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Text.Printf
import Data.List
import Data.Char

import Paths
import ReadResult
import qualified BenchmarkSettings as S

data ClientSettings = ClientSettings
   { title :: String
   , revisionInfo :: String
   , diffLink :: Maybe String
   }
 deriving (Generic)
instance ToJSON ClientSettings

data GlobalReport = GlobalReport
    { settings :: Maybe ClientSettings
    , benchmarks :: Maybe (M.Map BenchName ())
    , revisions :: Maybe (M.Map Hash RevReport)
    , benchGroups :: Maybe [BenchGroup]
    , branches :: Maybe (M.Map BranchName BranchReport)
    }

instance ToJSON GlobalReport where
    toJSON (GlobalReport {..}) = object $
        ( "settings"    .=? settings ) ++
        ( "benchmarks"  .=? benchmarks ) ++
        ( "revisions"   .=? revisions ) ++
        ( "benchGroups" .=? benchGroups ) ++
        ( "branches"    .=? branches )
      where
        k .=? Just v  = [ k .= toJSON v ]
        _ .=? Nothing = []


emptyGlobalReport :: GlobalReport
emptyGlobalReport = GlobalReport Nothing Nothing Nothing Nothing Nothing


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

data Status = Built | Failed | Waiting
 deriving (Show, Generic)
instance ToJSON Status
instance FromJSON Status

data Summary = Summary
    { hash :: Hash
    , gitDate :: Integer
    , gitSubject :: String
    , stats :: SummaryStats
    , parents :: [Hash]
--    , status :: Status
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

data BranchReport = BranchReport
    { branchHash :: Hash
    , mergeBaseHash :: Hash
    , branchStats :: SummaryStats
    , commitCount :: Int
    }

 deriving (Generic)
instance ToJSON BranchReport
instance FromJSON BranchReport

data ChangeType = Improvement | Boring | Regression
 deriving (Eq, Generic)
instance ToJSON ChangeType
instance FromJSON ChangeType

data BenchGroup = BenchGroup
    { groupName :: String
    , groupMembers :: [BenchName]
    , groupUnitFull :: Maybe String
    }
 deriving (Generic)
instance ToJSON BenchGroup
instance FromJSON BenchGroup

data BenchResult = BenchResult
    { name :: String
    , value :: BenchValue
    , unit :: String
    , important :: Bool
    }
 deriving (Generic)
instance ToJSON BenchResult where
    toJSON = genericToJSON defaultOptions
#if MIN_VERSION_aeson(0,10,0)
    toEncoding = genericToEncoding defaultOptions
#endif
instance FromJSON BenchResult where
    parseJSON = genericParseJSON defaultOptions

data BenchComparison = BenchComparison
    { changeName :: String
    , change :: String
    , changeType :: ChangeType
    , changeImportant :: Bool
    }

-- A smaller BenchResult
-- (This is a hack: BenchResult no longer carries a changeType. But it is convenient to
-- have that in the graph report, for the tallying for the graph summary. But all not very
-- satisfactory.)
data GraphPoint = GraphPoint
    { gpValue :: BenchValue
    , gpChangeType :: ChangeType
    }
 deriving (Generic)
instance ToJSON GraphPoint where
    toJSON = genericToJSON graphPointOptions
#if MIN_VERSION_aeson(0,10,0)
    toEncoding = genericToEncoding graphPointOptions
#endif
instance FromJSON GraphPoint where
    parseJSON = genericParseJSON graphPointOptions

graphPointOptions = defaultOptions { fieldLabelModifier = fixup }
  where fixup ('g':'p':c:cs) = toLower c : cs

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
    typ | perc >= 0,  perc < th_up = Boring
        | perc <  0, -perc < th_down = Boring
        | perc  >= 0 = Improvement
        | perc  <  0 = Regression

    perc = 100 * ((f2 - f1) / f1)
    th_up = S.threshold s
    -- Adjusted threshold, to make sure that the inverse change is flagged
    -- equivalently
    th_down = (1-(1/(1+S.threshold s/100)))*100

toFloat :: BenchValue -> Double
toFloat (I i) = fromIntegral i
toFloat (F f) = f

explain :: S.BenchSettings -> BenchValue -> BenchValue -> (String, ChangeType)
explain s@(S.numberType -> S.SmallIntegralNT) (I i1) (I i2) = explainSmallInt s i1 i2
explain s@(S.numberType -> S.IntegralNT)      (I i1) (I i2) = explainInt s i1 i2
-- Treat everything else as Floats, so that we do something sensible
-- even if the user did not set the numberType correctly:
explain s                                     v1     v2     = explainFloat s (toFloat v1) (toFloat v2)

toResult :: S.BenchSettings -> String -> BenchValue -> BenchResult
toResult s name value = BenchResult
    { name      = name
    , value     = value
    , unit      = S.unit s
    , important = S.important s
    }

makeComparison :: S.BenchSettings -> String -> BenchValue -> Maybe BenchValue -> BenchComparison
makeComparison s name value prev = BenchComparison
    { changeName      = name
    , change          = change
    , changeType      = changeType
    , changeImportant = S.important s
    }
  where
    (change, changeType') =
        case prev of
            Just p -> explain s p value
            Nothing -> noExplanation
    changeType | S.smallerIsBetter s = invertChangeType changeType'
               | otherwise           =                  changeType'

toSummaryStats :: [BenchComparison] -> SummaryStats
toSummaryStats comps = SummaryStats
    { totalCount = length comps
    , improvementCount = length
        [ () | comp <- importantComps , changeType comp == Improvement ]
    , regressionCount =  length
        [ () | comp <- importantComps , changeType comp == Regression ]
    , summaryDesc = andMore "No significant changes" 5
        [ changeName comp ++ ": " ++ change comp
        | comp <- importantComps
        , changeType comp `elem` [Improvement, Regression]
        ]
    }
  where importantComps = filter changeImportant comps

andMore :: String -> Int -> [String] -> String
andMore def _ [] = def
andMore _   n xs = intercalate "\n" (take n xs) ++ rest
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

createBranchReport ::
    S.Settings -> Hash -> Hash ->
    ResultMap -> ResultMap ->
    Int ->
    BranchReport
createBranchReport settings this other thisM otherM commitCount = BranchReport
    { branchHash = this
    , mergeBaseHash = other
    , branchStats = toSummaryStats comparisons
    , commitCount = commitCount
    }
  where
    comparisons =
        [ makeComparison s name value (M.lookup name otherM)
        | (name, value) <- M.toAscList thisM
        , let s = S.benchSettings settings name
        ]

createReport ::
    S.Settings -> Hash -> [Hash] ->
    ResultMap -> ResultMap ->
    String -> String -> Integer ->
    RevReport
createReport settings this parents thisM parentM log subject date = RevReport
    { summary = Summary
        { hash = this
        , parents = parents
        , stats = toSummaryStats comparisons
        , gitSubject = subject
        , gitDate = date
        }
    --, benchGroups = benchGroups
    , benchResults = results
    , gitLog = log
    }
  where
    results = M.fromList
        [ (name, toResult s name value)
        | (name, value) <- M.toAscList thisM
        , let s = S.benchSettings settings name
        ]
    comparisons =
        [ makeComparison s name value (M.lookup name parentM)
        | (name, value) <- M.toAscList thisM
        , let s = S.benchSettings settings name
        ]

summarize :: RevReport -> Summary
summarize (RevReport {..}) = summary
