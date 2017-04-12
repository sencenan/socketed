{-# LANGUAGE ScopedTypeVariables #-}

module Network.Socketed.Internal where

import Conduit (
      ConduitM, MonadIO,
      (.|), runConduit, sourceHandle, stdoutC
   )

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, race, wait)
import Control.Exception (AsyncException(..), catch)

import Data.ByteString (ByteString)
import Data.Either.Utils (fromRight)
import qualified Data.Conduit.Binary as CB (lines)

import Language.Haskell.TH (stringE)
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import System.IO (stdin)

data SocketedOptions = SocketedOptions {
      initWait :: Int,
      port :: Int,
      host :: String
   }

stdinLines :: MonadIO m => ConduitM a ByteString m ()
stdinLines = sourceHandle stdin .| CB.lines

stdinPassthrough :: IO ()
stdinPassthrough = runConduit $ sourceHandle stdin .| stdoutC

withStdinPassthrough :: IO a -> IO a
withStdinPassthrough work = do
   a <- async work
   stdinPassthrough
   wait a

showWSHost :: String -> Int -> String
showWSHost h p = "ws://" ++ h ++ ":" ++ show p

stringQuote :: QuasiQuoter
stringQuote = QuasiQuoter {
   quoteExp = stringE,
   quotePat = undefined,
   quoteType = undefined,
   quoteDec = undefined
}

waitTimeout :: forall a . Async a -> a -> Int -> IO a
waitTimeout task def time =
   let
      left = threadDelay time >> cancel task
      right = wait task
      hdl :: AsyncException -> IO a
      hdl _ = return def
   in
      fmap fromRight (race left right) `catch` hdl
