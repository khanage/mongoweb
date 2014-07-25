-- | Application entry point

{-# LANGUAGE RecordWildCards #-}

module Main where

import           Configuration
import           Data.Configurator (Worth (..), load)
import           Data.Mongo
import           Database.MongoDB
import           Web.Routing

main :: IO ()
main = do
  conf <- load [Required "app.config"]

  WebConf   {..} <- loadWebConf conf
  MongoConf {..} <- loadMongoConf conf

  let scottMsg = "Will run scotty on port: " ++ show wPort
      mongoMsg = "Will run mongo as:\n"
        ++ "\tConnection: " ++ show mConnection ++ "\n"
        ++ "\tDatabase:   " ++ show mDatabase   ++ "\n"
        ++ "\tCollection: " ++ show mCollection ++ "\n"

  putStrLn "Starting up\n"
  putStrLn scottMsg
  putStrLn mongoMsg

  mongoPipe <- mongoConnect mConnection

  let mongoRunner = createRunner mongoPipe master mDatabase
      upserter = upsertItem mCollection mongoRunner
      deleter  = deleteItem mCollection mongoRunner

  putStrLn "Connected to mongo"
  
  webApplication upserter deleter wPort

  putStrLn "Done"
