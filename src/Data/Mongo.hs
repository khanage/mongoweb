-- | Module for all access to mongo

{-# LANGUAGE OverloadedStrings #-}

module Data.Mongo where

import qualified Data.Text        as T
import           Database.MongoDB

type MongoId = T.Text

type DocumentWithId = (MongoId, Document)

type Upserter = DocumentWithId -> IO ()
type Deleter  = MongoId -> IO ()

type Runner m a = Action m a -> m a

createRunner :: Pipe -> AccessMode -> Database -> Action IO a -> IO a
createRunner = access

upsertItem :: Collection -> Runner IO () -> Upserter
upsertItem collName runner (docId, doc) =
  let knownGoodDoc = merge ["_id" =: docId] doc
  in runner $ save collName knownGoodDoc

deleteItem :: Collection -> Runner IO () -> Deleter
deleteItem collName runner docId = runner $ deleteOne $ Select ["_id" =: docId] collName
