{-# LANGUAGE QuasiQuotes #-}

module Network.Socketed.Application.Ficketed.Template (refreshHtml) where

import Network.Socketed.Internal (showWSHost, stringQuote)
import Network.Socketed.Template (socketedScript)
import Network.Socketed.Application.Ficketed.Internal (FicketedOptions(..))

refreshHtml :: String -> FicketedOptions -> String
refreshHtml path (FicketedOptions _ _ _ wp wh) = socketedScript
   (showWSHost wh wp)
   0
   (
      [stringQuote|
         function(event) {
      |]
      ++ "var path ='" ++ path ++ "';" ++
      [stringQuote|
            if (event.data === path || '/' + event.data === path) {
               window.location.reload();
            }
         }
      |]
   )
   [stringQuote| function(e) { console.error(e); } |]
