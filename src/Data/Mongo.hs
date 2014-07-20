-- | Module for all access to mongo
{-# LANGUAGE OverloadedStrings #-}
module Data.Mongo where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Bson
import qualified Data.Text              as T
import           Database.MongoDB

type MongoId = T.Text

type DocumentWithId = (MongoId, Document)

type Runner m a = Action m a -> m a

createRunner :: Pipe -> AccessMode -> Database -> Action IO a -> IO a
createRunner pipe accessMode database = access pipe accessMode database

upsertItem :: Collection -> Runner IO () -> DocumentWithId -> IO ()
upsertItem coll runner (id,doc) =
  let knownGoodDoc = merge ["_id" =: id] doc
  in runner $ save coll knownGoodDoc

deleteItem :: Collection -> Runner IO () -> MongoId -> IO ()
deleteItem coll runner id = runner $ deleteOne $ Select ["_id" =: id] coll
