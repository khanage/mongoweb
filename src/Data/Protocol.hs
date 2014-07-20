{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- | On the wire protocol extraction
module Data.Protocol (Message (..), unprotocol) where

import           Control.Applicative
import           Control.Monad        (liftM, mzero)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.BsonAesonBridge (toDocument)
import qualified Data.ByteString.Lazy as B
import           Data.Mongo
import qualified Data.Text            as T

data Message = Delete MongoId
             | Upsert DocumentWithId

unprotocol :: B.ByteString -> Maybe Message
unprotocol bs = do
  Protocol {..} <- decode' bs
  case pMethod of
    UpsertMethod -> (Upsert . (pId,)) `liftM` toDocument doc
    DeleteMethod -> return $ Delete pId

-- | Private implementations

data Method = UpsertMethod | DeleteMethod
instance FromJSON Method where
  parseJSON (String t) =
    case T.toLower t of
      "upsert" -> return UpsertMethod
      "delete" -> return DeleteMethod
      otherwise -> mzero
  parseJSON _ = mzero

data Protocol = Protocol { pMethod :: Method, pId :: MongoId, doc :: Value }
instance FromJSON Protocol where
  parseJSON (Object o) =
    Protocol      <$>
    o .: "method" <*>
    o .: "id"     <*>
    o .: "body"
  parseJSON _ = mzero
