-- | Load application configuration

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Configuration where

import           Prelude          hiding (lookup)
import           Control.Monad           (liftM)
import           Data.Configurator       (lookupDefault, require, lookup)
import           Data.Configurator.Types (Config)
import qualified Data.Text               as T

-- | Scotty configuration

data WebConf = WebConf { wPort :: Int }

loadWebConf :: Config -> IO WebConf
loadWebConf config = WebConf `liftM` lookupDefault 3000 config "web.port"

-- | Mongo configuration

data MongoConnection = MongoDirectHost { mcHost :: String }
                     | MongoReplicaSet { mcReplicaSet :: String
                                       , mcReplicaMembers :: [String] }
                       deriving (Show)

data MongoConf = MongoConf { mConnection :: MongoConnection
                           , mDatabase   :: T.Text
                           , mCollection :: T.Text }

loadMongoConf :: Config -> IO MongoConf
loadMongoConf config = do
  mDatabase   <- require config "mongo.databaseName"
  mCollection <- require config "mongo.collectionName"
  mConnection <- loadConnection config
  return MongoConf { .. }

loadConnection :: Config -> IO MongoConnection
loadConnection config = do
  mv <- lookup config "mongo.host"
  case mv of
    Just mcHost -> return MongoDirectHost { .. }
    Nothing     -> do
      mcReplicaMembers <- require config "mongo.replicaSetHosts"          
      mcReplicaSet     <- require config "mongo.replicaSet"
      
      return MongoReplicaSet { .. }
