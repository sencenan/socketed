module Network.Socketed.Application.Plocketed (
      PlocketedOptions(..), runPlocketedServer
   ) where

import Network.Socketed.Application.Plocketed.Internal (PlocketedOptions(..))

runPlocketedServer :: PlocketedOptions -> IO ()
runPlocketedServer _ = putStrLn "hello"
