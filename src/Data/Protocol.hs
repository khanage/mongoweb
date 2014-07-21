{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- | On the wire protocol extraction
module Data.Protocol (Message (..), unprotocol) where

import           Control.Applicative
import           Control.Monad        (mzero)
import           Data.Aeson
import           Data.BsonAesonBridge (toDocument)
import qualified Data.ByteString.Lazy as B
import           Data.Mongo
import qualified Data.Text            as T
import           Debug.Trace

data Message = Delete MongoId
             | Upsert DocumentWithId
               deriving (Show, Eq)

unprotocol :: B.ByteString -> Maybe Message
unprotocol bs = do
  Protocol {..} <- decode' bs
  case pMethod of
    UpsertMethod -> do
      body <- traceShowMessage "pDoc is:" pDoc
      bDoc <- toDocument body
      return $ Upsert (pId, bDoc)
    DeleteMethod -> return $ Delete pId

-- | Private implementations

traceShowMessage :: (Show a) => String -> a -> a
traceShowMessage s a = trace (s ++ " " ++ show a) a

data Method = UpsertMethod | DeleteMethod deriving (Show,Eq)
instance FromJSON Method where
  parseJSON (String t) =
    case T.toLower t of
      "upsert" -> return UpsertMethod
      "delete" -> return DeleteMethod
      _        -> mzero
  parseJSON _ = mzero

data Protocol = Protocol { pMethod :: Method, pId :: MongoId, pDoc :: Maybe Value } deriving (Show,Eq)
instance FromJSON Protocol where
  parseJSON (Object o) =
    Protocol      <$>
    o .:  "method" <*>
    o .:  "id"     <*>
    o .:? "body"
  parseJSON _ = mzero
