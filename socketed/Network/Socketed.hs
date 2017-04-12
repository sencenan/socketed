module Network.Socketed (
      SocketedOptions(..), runSocketedServer
   ) where

import Conduit (
      ConduitM, MonadIO,
      (.|), mapM_C, runConduit, sinkList, takeExactlyC
   )
import Data.Conduit.TMChan (TMChan, sinkTMChan, dupTMChan, sourceTMChan)

import Control.Concurrent.Async (async)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMChan (newBroadcastTMChanIO)

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (fromStrict)
import Data.Void (Void)

import Network.HTTP.Types (status200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (
      Settings,
      defaultSettings, setPort, setHost, runSettings
   )
import Network.Wai.Handler.WebSockets (websocketsOr)

import Network.WebSockets (
      ServerApp,
      acceptRequest, defaultConnectionOptions, sendTextData
   )

import Network.Socketed.Internal (SocketedOptions(..), stdinLines)
import Network.Socketed.Template (evalHtml)

sinkStdinToChan :: MonadIO m => TMChan ByteString -> ConduitM a Void m ()
sinkStdinToChan = (stdinLines .|) . flip sinkTMChan False

replayedLines :: SocketedOptions -> IO [ByteString]
replayedLines (SocketedOptions r _ _) = runConduit
   $ stdinLines .| takeExactlyC r sinkList

serverSettings :: SocketedOptions -> Settings
serverSettings (SocketedOptions _ p h) = setHost (read hoststr)
   $ setPort p defaultSettings where hoststr = "Host \"" ++ h ++ "\""

socketedApp :: [ByteString] -> ByteString -> TMChan ByteString -> Application
socketedApp rls html broadcastChan
   = websocketsOr defaultConnectionOptions wsApp backupApp where
      wsApp :: ServerApp
      wsApp pendingConn = do
         conn <- acceptRequest pendingConn
         chan <- atomically $ dupTMChan broadcastChan
         _ <- mapM (sendTextData conn) rls -- send replayed lines
         runConduit $ sourceTMChan chan .| mapM_C (sendTextData conn)

      backupApp :: Application
      backupApp _ respond = respond
         $ responseLBS status200 [] (fromStrict html)

runSocketedServer :: SocketedOptions -> IO ()
runSocketedServer opts = do
   rls <- replayedLines opts
   chan <- newBroadcastTMChanIO
   _ <- async . runConduit $ sinkStdinToChan chan
   let app = socketedApp rls (pack $ evalHtml opts) chan
   runSettings (serverSettings opts) app
