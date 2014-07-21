-- | Web application

{-# LANGUAGE OverloadedStrings #-}

module Web.Routing (webApplication) where

import           Control.Monad          (liftM)
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe
import           Data.Mongo
import           Data.Protocol
import qualified Data.Text.Lazy         as T
import           Network.HTTP.Types
import           Web.Scotty

type Port = Int

webApplication :: Upserter -> Deleter -> Port -> IO ()
webApplication upserter deleter port =
  let dispatcher = dispatchMessage upserter deleter
  in scotty port $ do
    get "/health" $ text "Ok"

    post "/message" $ do
      rawBody <- body

      let message = unprotocol rawBody
          success = dispatcher `liftM` message

      success `otherwiseM` badRequest

dispatchMessage :: Upserter -> Deleter -> Message -> ActionM ()
dispatchMessage upserter deleter message =
  case message of
    Delete mongoId -> do
      liftIO $ deleter mongoId
      packable $ "Deleted" ++ show mongoId

    Upsert docWithId@(docId,_) -> do
      liftIO $ upserter docWithId
      packable $ "Inserted " ++ show docId

badRequest :: ActionM ()
badRequest = do
  status status400
  text "Bad request"

otherwiseM :: Maybe a -> a -> a
otherwiseM = flip fromMaybe

packable :: String -> ActionM ()
packable = text . T.pack
