module Network.Socketed (
      SocketedOptions(..), runSocketedServer
   ) where

import Conduit (
      ConduitM, MonadIO,
      (.|), mapM_C, runConduit, sourceHandle, sinkList, takeExactlyC
   )
import Data.Conduit.TMChan (TMChan, sinkTMChan, dupTMChan, sourceTMChan)
import qualified Data.Conduit.Binary as CB

import Control.Concurrent.Async (async)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMChan (newBroadcastTMChanIO)

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (fromStrict)
import Data.Void (Void)

import System.IO (stdin)

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

data SocketedOptions = SocketedOptions {
      replayAmount :: Int,
      port :: Int,
      host :: String
   }

stdinLines :: MonadIO m => ConduitM a ByteString m ()
stdinLines = sourceHandle stdin .| CB.lines

sinkStdinToChan :: MonadIO m => TMChan ByteString -> ConduitM a Void m ()
sinkStdinToChan = (stdinLines .|) . flip sinkTMChan False

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

settings :: SocketedOptions -> Settings
settings (SocketedOptions _ p h) = setHost (read hoststr)
   $ setPort p defaultSettings
      where hoststr = "Host \"" ++ h ++ "\""

showHost :: SocketedOptions -> String
showHost (SocketedOptions _ p h) = "ws://" ++ h ++ ":" ++ show p

replayedLines :: SocketedOptions -> IO [ByteString]
replayedLines (SocketedOptions r _ _) = runConduit
   $ stdinLines .| takeExactlyC r sinkList

runSocketedServer :: SocketedOptions -> IO ()
runSocketedServer opts = do
   rls <- replayedLines opts
   chan <- newBroadcastTMChanIO
   _ <- async . runConduit $ sinkStdinToChan chan
   let app = socketedApp rls (backupkHtml opts) chan
   _ <- putStrLn $ showHost opts
   runSettings (settings opts) app

backupkHtml :: SocketedOptions -> ByteString
backupkHtml opts@(SocketedOptions r _ _) = pack $ unlines [
      "<!DOCTYPE html><html><body><script>",
      "(function(host, linesToDrop) {",
      "  window.handleMessage = function(event) {",
      "     eval(event.data);",
      "  };",
      "",
      "  window.handleMessageError = function(event, error) {",
      "     var c = document.createElement('div');",
      "     c.textContent = 'failed to eval: '+ event.data;",
      "     window.document.body.appendChild(c);",
      "  };",
      "",
      "  var retryCount;",
      "",
      "  var connect = function(l) {",
      "     var",
      "        lineDropped = 0,",
      "        socket = new WebSocket(host);",
      "",
      "     socket.onopen = function(e) {",
      "        console.log('connected');",
      "        retryCount = 0;",
      "     };",
      "",
      "     socket.onclose = function(e) {",
      "        console.log('socket closed: ', e);",
      "",
      "        if (++retryCount > 20) {",
      "           window.document.body.textContent = 'disconnected...';",
      "        } else {",
      "           setTimeout(function() {",
      "              console.log('reconnecting...');",
      "              connect(l);",
      "           }, 2000);",
      "        }",
      "     };",
      "",
      "     socket.onmessage = function(event) {",
      "        if (++lineDropped > linesToDrop) {",
      "           try {",
      "              window.handleMessage(event);",
      "           } catch(error) {",
      "              window.handleMessageError(event, error);",
      "           }",
      "        } else {",
      "           console.log('drops: ', event.data)",
      "        }",
      "     };",
      "  };",
      "",
      "  connect(0);",
      "})('" ++ showHost opts ++ "', " ++ show r ++ ");",
      "</script></body></html>"
   ]
