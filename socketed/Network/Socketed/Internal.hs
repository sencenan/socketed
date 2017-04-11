module Network.Socketed.Internal where

import Language.Haskell.TH (stringE)
import Language.Haskell.TH.Quote (QuasiQuoter(..))

data SocketedOptions = SocketedOptions {
      replayAmount :: Int,
      port :: Int,
      host :: String
   }

showHost :: SocketedOptions -> String
showHost (SocketedOptions _ p h) = "ws://" ++ h ++ ":" ++ show p

stringQuote :: QuasiQuoter
stringQuote = QuasiQuoter {
   quoteExp = stringE,
   quotePat = undefined,
   quoteType = undefined,
   quoteDec = undefined
}
