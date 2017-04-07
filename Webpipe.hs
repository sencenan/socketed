{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy as BL
import Data.Void (Void)
import Data.Text (Text)
import Data.Semigroup ((<>))

import Conduit (
      ConduitM, MonadIO,
      (.|), mapM_C, runConduit, sourceHandle, sinkList, takeExactlyC, yieldMany
   )
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

-- options
import qualified Options.Applicative as Opt
import Options.Applicative ((<**>))

mkApp :: [ByteString] -> TMChan ByteString -> Application
mkApp replayedLines broadcastChan = websocketsOr defaultConnectionOptions wsApp backupApp
   where
      wsApp :: ServerApp
      wsApp pending_conn = do
         conn <- acceptRequest pending_conn
         chan <- atomically $ dupTMChan broadcastChan
         mapM (sendTextData conn) replayedLines -- send replayed lines
         runConduit $ sourceTMChan chan .| mapM_C (sendTextData conn)

      backupApp :: Application
      backupApp _ respond = respond
         $ responseLBS status200 [] (BL.fromStrict (mkHtml . length $ replayedLines))

stdinLines :: MonadIO m => ConduitM a ByteString m ()
stdinLines = (sourceHandle stdin) .| CB.lines

sinkStdinToChan :: MonadIO m => TMChan ByteString -> ConduitM a Void m ()
sinkStdinToChan = (stdinLines .|) . flip sinkTMChan False

data Params = Params { replayAmt :: Int }

params :: Opt.Parser Params
params = Params
   <$> Opt.option Opt.auto
      ( Opt.long "replay"
      <> Opt.help "number of lines to replay on client connect"
      <> Opt.showDefault
      <> Opt.value 1
      <> Opt.metavar "INT" )

main :: IO ()
main = go =<< Opt.execParser opts
   where
      opts = Opt.info (Opt.helper <*> params)
         (
            Opt.fullDesc
            <> Opt.progDesc "socketed"
            <> Opt.header "socketed"
         )

-- missing initial data from the stdin
go :: Params -> IO ()
go (Params replayAmt) = do
   replayedLines <- runConduit $ stdinLines .| takeExactlyC replayAmt sinkList
   chan <- newBroadcastTMChanIO
   async . runConduit $ sinkStdinToChan chan
   app <- return $ mkApp replayedLines chan
   putStrLn "Starting at 3000"
   run 3000 app

mkHtml :: Int -> ByteString
mkHtml replayAmt = pack $ unlines [
      "<!DOCTYPE html>",
      "<html>",
      "   <head>",
      "      <title>Testing page for websockets</title>",
      "   </head>",
      "   <body>",
      "      <pre id='pre'></pre>",
      "      <script>",
      "         var e = document.getElementById('pre');",
      "",
      "         var connect = function(linesToDrop) {",
      "            var",
      "               lineDropped = 0,",
      "               socket = new WebSocket('ws://127.0.0.1:3000');",
      "",
      "            socket.onclose = function(e) {",
      "               console.log('socket closed: ', e);",
      "               setTimeout(function() { connect( "
         ++ show replayAmt ++ " ) }, 5000);",
      "            };",
      "",
      "            socket.onmessage = function(event) {",
      "               if (++lineDropped > linesToDrop) {",
      "                  e.textContent += event.data + '\\n';",
      "               } else {",
      "                  console.log('drops replayed line: ', event.data)",
      "               }",
      "            };",
      "         };",
      "",
      "         connect(0);",
      "      </script>",
      "   </body>",
      "</html>"
   ]

