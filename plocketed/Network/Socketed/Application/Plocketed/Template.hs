{-# LANGUAGE QuasiQuotes #-}

module Network.Socketed.Application.Plocketed.Template (plotHtml) where

import Network.Socketed.Internal (showWSHost, stringQuote)
import Network.Socketed.Template (socketedScript)
import Network.Socketed.Application.Plocketed.Internal (PlocketedOptions(..))

plotHtml :: PlocketedOptions -> String
plotHtml opts@(PlocketedOptions _ _ wp wh) = socketedScript
   (showWSHost wh wp)
   0
   (
      [stringQuote|
         function(e) {
            console.log(e)
         }
      |]
   )
   [stringQuote| function(e) { console.error(e); } |]
