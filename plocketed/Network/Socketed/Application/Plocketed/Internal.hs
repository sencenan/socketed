module Network.Socketed.Application.Plocketed.Internal where

data PlocketedOptions = PlocketedOptions {
      port :: Int,
      host :: String,
      wsPort :: Int,
      wsHost :: String
   }
