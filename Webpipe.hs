{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString
import qualified Data.ByteString.Lazy as BL

import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL

import Control.Concurrent.Async
import Control.Monad.IO.Class

import System.IO (stdin)

-- html/warp
import Network.Wai
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)

-- -- websockets
-- import Control.Monad (forever)
-- import qualified Data.Text as T
-- import qualified Network.WebSockets as WS

stdinSrc :: Source IO ByteString
stdinSrc = CB.sourceHandle stdin

runHttpServer :: ByteString -> IO (Async ())
runHttpServer = async . run 3000 . mkHttpApp

mkHttpApp :: ByteString -> Application
mkHttpApp resp _ respond = respond $
   responseLBS status200 [("Content-Type", "text/html")] (BL.fromStrict resp)

serveWeb :: Conduit ByteString IO (ByteString, Async ())
serveWeb = await >>= maybe (return ()) f
   where
      f = (g =<<) . liftIO . runHttpServer
      g = CL.mapM . (return .) . flip (,)

printLog :: Conduit (ByteString, Async ()) IO (IO(), Async ())
printLog = CL.mapM f where f (x, y) = return (print x, y)

waitSink :: Consumer (t, Async ()) IO (Async ())
waitSink = await >>= maybe waitSink f where f (_, a) = return a

process :: IO (Async ())
process = stdinSrc =$= CB.lines =$= serveWeb =$= printLog $$ waitSink

-- main :: IO ()
--main = process >>= wait
main = stdinSrc =$= CB.lines =$= serveWeb =$= printLog $$ CL.take 10000
