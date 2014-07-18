-- | Application entry point
module Main where

import           Configuration
import           Control.Monad.Error (runErrorT)
import           Data.Configurator   (Worth (..), load)

main = do
  conf <- load [Required "app.config"]
  scottyConf <- loadWebConf conf
  mongoConf <- loadMongoConf conf

  let webMsg = "Will run scotty on port " ++ show (wPort scottyConf)
      mongoMsg =
        "Will run mongo as:\n"
        ++ "\tConnection: " ++ show (mConnection mongoConf) ++ "\n"
        ++ "\tDatabase: "   ++ show (mDatabase mongoConf)   ++ "\n"
        ++ "\tCollection: " ++ show (mCollection mongoConf) ++ "\n"

  putStrLn "Starting up\n"
  putStrLn webMsg
  putStrLn mongoMsg

  putStrLn "Done"
