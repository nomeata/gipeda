{-# LANGUAGE OverloadedStrings #-}

module JsonUtils where

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.Aeson
import Data.List (foldl')

mapObject :: (Object -> Object) -> Value -> Value
mapObject f (Object m) = Object (f m)
mapObject _ v =  error $ "Not an object " ++ show v

delete :: [T.Text] -> Value -> Value
delete []       = error "delete []"
delete ["*"]    = error "delete [\"*\"]"
delete [k]      = mapObject $ HM.delete k
delete ("*":ks) = mapObject $ HM.map (delete ks)
delete (k:ks)   = mapObject $ HM.adjust (delete ks) k

merge :: Value -> Value -> Value
merge (Object m1) (Object m2) = Object $ HM.unionWith merge m2 m1
merge v1 v2 = error $ "Cannot merge " ++ show v1 ++ " with " ++ show v2

merges :: [Value] -> Value
merges = foldl' merge (Object HM.empty)



