{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Void (Void)
import Data.Text (Text)

import Conduit (ConduitM, MonadIO, runConduit, mapM_C, sourceHandle, (.|))
import Data.Conduit.TMChan (TMChan, sinkTMChan, dupTMChan, sourceTMChan)
import qualified Data.Conduit.Binary as CB

import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMChan (newBroadcastTMChanIO)

import System.IO (stdin)

-- html/warp
import Network.Wai (Application, responseLBS)
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)

-- websockets
import Network.WebSockets (
      ServerApp,
      acceptRequest, defaultConnectionOptions, sendTextData
   )

mkApp :: TMChan ByteString -> Application
mkApp broadcastChan = websocketsOr defaultConnectionOptions wsApp backupApp
   where
      wsApp :: ServerApp
      wsApp pending_conn = do
         conn <- acceptRequest pending_conn
         chan <- atomically $ dupTMChan broadcastChan
         runConduit $ sourceTMChan chan .| CB.lines .| mapM_C (sendTextData conn)

      backupApp :: Application
      backupApp _ respond = respond $ responseLBS status200 [] "Not a WebSocket request"

sinkStdinToChan :: MonadIO m => TMChan ByteString -> ConduitM a Void m ()
sinkStdinToChan = (sourceHandle stdin .|) . flip sinkTMChan True

-- missing initial data from the stdin
main :: IO ()
main = do
   chan <- newBroadcastTMChanIO
   a <- async . runConduit $ sinkStdinToChan chan
   app <- return $ mkApp chan
   putStrLn "Starting at 3000"
   run 3000 app
   wait a
