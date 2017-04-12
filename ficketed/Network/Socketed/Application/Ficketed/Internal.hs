module Network.Socketed.Application.Ficketed.Internal where

data FicketedOptions = FicketedOptions {
      dir :: Maybe String,
      port :: Int,
      host :: String,
      wsPort :: Int,
      wsHost :: String,
      skippedMimes :: String,
      extraMimes :: String,
      htmlMimes :: String,
      textExts :: String
   }
