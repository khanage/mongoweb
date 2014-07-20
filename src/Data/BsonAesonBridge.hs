-- | Methods to help bridge the gap between Aeson and Bson types

{-# LANGUAGE OverloadedStrings #-}

module Data.BsonAesonBridge (toDocument) where

import           Control.Monad       (liftM)
import qualified Data.Aeson          as A
import           Data.Bson           ((=:))
import qualified Data.Bson           as B
import           Data.HashMap.Strict (foldrWithKey)
import           Data.Scientific
import qualified Data.Text           as T
import qualified Data.Vector         as V

toDocument :: A.Value -> Maybe B.Document
toDocument (A.Object o) = foldrWithKey addToDoc (Just []) o
toDocument _            = Nothing

addToDoc :: T.Text -> A.Value -> Maybe B.Document -> Maybe B.Document
addToDoc _ _ Nothing    = Nothing
addToDoc k v (Just doc) =
  case mapValue v of
    Just mapped -> Just $ B.merge doc [k =: mapped]
    Nothing -> Nothing

mapValue :: A.Value -> Maybe B.Value
mapValue av = case av of
  A.Null     -> Just B.Null
  A.Bool b   -> Just $ B.Bool b
  A.String s -> Just $ B.String s
  A.Number n -> convertScientific n
  A.Object o -> B.Doc `liftM` toDocument av
  A.Array as -> B.Array `liftM` convertArray as

convertArray :: V.Vector A.Value -> Maybe [B.Value]
convertArray as = sequence $ convertArrayMembers as

convertArrayMembers :: V.Vector A.Value -> [Maybe B.Value]
convertArrayMembers = flip V.foldr [] $ \a s -> mapValue a:s

convertScientific :: Scientific -> Maybe B.Value
convertScientific s =
  case floatingOrInteger s of
    Left float -> Just $ B.Float float
    Right int  -> Just $ B.Int64 int
