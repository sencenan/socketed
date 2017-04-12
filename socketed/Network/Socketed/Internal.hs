{-# LANGUAGE ScopedTypeVariables #-}

module Network.Socketed.Internal where

import Conduit (
      ConduitM, MonadIO,
      (.|), runConduit, sourceHandle, stdoutC, await, yield, leftover
   )

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, race, wait)
import Control.Exception (AsyncException(..), catch)

import Data.ByteString (ByteString, empty)
import Data.Either.Utils (fromRight)
import qualified Data.Conduit.Binary as CB (lines)

import Language.Haskell.TH (stringE)
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import System.IO (stdin)

data SocketedOptions = SocketedOptions {
      host :: String,
      port :: Int
   }

stdinLines :: MonadIO m => ConduitM a ByteString m ()
stdinLines = sourceHandle stdin .| CB.lines

takeUntil2Empty :: MonadIO m => ConduitM ByteString ByteString m ()
takeUntil2Empty =
      loop
   where
      loop = do
         a <- await
         b <- await
         case (a, b) of
            (Nothing, Nothing) -> loop
            (Just x, Nothing) -> leftover x >> loop
            (Nothing, Just x) -> leftover x >> loop
            (Just x, Just y)
               | x == empty && y == empty -> return ()
               | otherwise -> leftover y >> yield x >> loop

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
