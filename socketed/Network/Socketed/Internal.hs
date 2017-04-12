module Network.Socketed.Internal where

import Conduit (
      ConduitM, MonadIO,
      (.|), runConduit, sourceHandle, stdoutC
   )

import Control.Concurrent.Async (async, wait)

import Data.ByteString (ByteString)
import qualified Data.Conduit.Binary as CB (lines)

import Language.Haskell.TH (stringE)
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import System.IO (stdin)

data SocketedOptions = SocketedOptions {
      replayAmount :: Int,
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
