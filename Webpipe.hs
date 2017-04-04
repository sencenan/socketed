module Main where

import qualified Data.ByteString.Char8 as B

import Data.Conduit
import Control.Monad.IO.Class
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL

import System.IO (stdin)

stdinSrc :: Source IO B.ByteString
stdinSrc = CB.sourceHandle stdin

conduit :: Conduit B.ByteString IO String
conduit = CL.map B.unpack

stdoutSink :: Sink String IO ()
stdoutSink = awaitForever $ liftIO . putStrLn

main :: IO ()
main = stdinSrc $$ CB.lines =$ conduit =$ stdoutSink
