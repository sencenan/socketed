module Socketed (
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

import Data.Conduit.TMChan (TMChan, sinkTMChan, dupTMChan, sourceTMChan)

import System.IO (stdin)

import Network.Wai (Application, responseLBS)
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (
      HostPreference, Settings,
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
stdinLines = (sourceHandle stdin) .| CB.lines

sinkStdinToChan :: MonadIO m => TMChan ByteString -> ConduitM a Void m ()
sinkStdinToChan = (stdinLines .|) . flip sinkTMChan False

socketedApp :: [ByteString] -> ByteString -> TMChan ByteString -> Application
socketedApp rls html broadcastChan
   = websocketsOr defaultConnectionOptions wsApp backupApp where
      wsApp :: ServerApp
      wsApp pendingConn = do
         conn <- acceptRequest pendingConn
         chan <- atomically $ dupTMChan broadcastChan
         mapM (sendTextData conn) rls -- send replayed lines
         runConduit $ sourceTMChan chan .| mapM_C (sendTextData conn)

      backupApp :: Application
      backupApp _ respond = respond
         $ responseLBS status200 [] (fromStrict html)

settings :: SocketedOptions -> Settings
settings (SocketedOptions _ port host) = setHost (read hoststr)
   $ setPort port
   $ defaultSettings
      where hoststr = "Host \"" ++ host ++ "\""

printHost :: SocketedOptions -> IO ()
printHost (SocketedOptions _ port host) = putStrLn
   $ "ws://" ++ host ++ ":" ++ show port

replayedLines :: SocketedOptions -> IO [ByteString]
replayedLines (SocketedOptions replayAmount _ _) = runConduit
   $ stdinLines .| takeExactlyC replayAmount sinkList

runSocketedServer :: SocketedOptions -> IO ()
runSocketedServer opts = do
   rls <- replayedLines opts
   chan <- newBroadcastTMChanIO
   async . runConduit $ sinkStdinToChan chan
   app <- return $ socketedApp rls (backupkHtml opts) chan
   printHost opts
   runSettings (settings opts) app

backupkHtml :: SocketedOptions -> ByteString
backupkHtml (SocketedOptions replayAmount port _) = pack $ unlines [
      "<!DOCTYPE html><html><body>",
      "      <pre id='pre'></pre>",
      "      <script>",
      "         var e = document.getElementById('pre');",
      "",
      "         var connect = function(linesToDrop) {",
      "            var",
      "               lineDropped = 0,",
      "               socket = new WebSocket('ws://127.0.0.1:"
         ++ show port ++ "');",
      "",
      "            socket.onclose = function(e) {",
      "               console.log('socket closed: ', e);",
      "               setTimeout(function() { connect( "
         ++ show replayAmount ++ " ) }, 5000);",
      "            };",
      "",
      "            socket.onmessage = function(event) {",
      "               if (lineDropped >= linesToDrop) {",
      "                  e.textContent += event.data + '\\n';",
      "               } else {",
      "                  lineDropped += 1;",
      "                  console.log('drops replayed line: ', event.data);",
      "               }",
      "            };",
      "         };",
      "",
      "         connect(0);",
      "      </script>",
      "</body></html>"
   ]
