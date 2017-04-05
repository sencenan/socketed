{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

import Data.Conduit
import Control.Monad.IO.Class
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL

import System.IO (stdin)

-- html/warp
import Network.Wai
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)

-- websockets
import Control.Monad (forever)
import qualified Data.Text as T
import qualified Network.WebSockets as WS

stdinSrc :: Source IO B.ByteString
stdinSrc = CB.sourceHandle stdin

conduit :: Conduit B.ByteString IO B.ByteString
conduit = CL.map id

-- stdoutSink :: Sink String IO ()
-- stdoutSink = awaitForever $ liftIO . putStrLn

svrSink :: Sink B.ByteString IO ()
svrSink = CL.mapM_ $ \f -> run 3000 (mkApp f)

mkApp resp = \_ respond -> respond $
   responseLBS status200 [("Content-Type", "text/html")] (BL.fromStrict resp)

main :: IO ()
main = stdinSrc $$ CB.lines =$ conduit =$ svrSink
