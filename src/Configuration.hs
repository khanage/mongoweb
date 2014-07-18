-- | Load application configuration
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Configuration where

import           Control.Monad           (liftM)
import           Data.Configurator       (Worth (..), lookupDefault, require)
import           Data.Configurator.Types (Config)
import qualified Data.Text               as T

data MongoConf = MongoConf { mConnection :: T.Text
                           , mDatabase   :: T.Text
                           , mCollection :: T.Text }

loadMongoConf :: Config -> IO MongoConf
loadMongoConf config = do
  mConnection <- require config "mongo.connectionString"
  mDatabase   <- require config "mongo.databaseName"
  mCollection <- require config "mongo.collectionName"
  return MongoConf { .. }

data WebConf = WebConf { wPort :: Int }

loadWebConf config = WebConf `liftM` lookupDefault 3000 config "web.port"
