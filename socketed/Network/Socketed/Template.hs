{-# LANGUAGE QuasiQuotes #-}

module Network.Socketed.Template (socketedScript, evalHtml) where

import Data.List (intercalate)

import Network.Socketed.Internal (SocketedOptions(..), showWSHost, stringQuote)

socketedScript :: String -> Int -> String -> String  -> String
socketedScript h ldrop hf ef = [stringQuote|
   <script>
   (function(host, linesToDrop, handleMessage, handleMessageError) {
      window.handleMessage = handleMessage;
      window.handleMessageError = handleMessageError;

      var retryCount;

      var connect = function(l) {
         var
            lineDropped = 0,
            socket = new WebSocket(host);

         socket.onopen = function(e) {
            console.log('connected');
            retryCount = 0;
         };

         socket.onclose = function(e) {
            console.log('socket closed: ', e);

            if (++retryCount > 20) {
               window.document.body.textContent = 'disconnected...';
            } else {
               setTimeout(function() {
                  console.log('reconnecting...');
                  connect(l);
               }, 2000);
            }
         };

         socket.onmessage = function(event) {
            if (++lineDropped > linesToDrop) {
               try {
                  window.handleMessage(event);
               } catch(error) {
                  window.handleMessageError(event, error);
               }
            } else {
               console.log('drops: ', event.data)
            }
         };
      };

      connect(0);
   })(|]
   ++ intercalate "," ["'" ++ h ++ "'", show ldrop, hf, ef]
   ++ [stringQuote|)</script>|]

wrapHtml :: String -> String
wrapHtml inner = [stringQuote|
      <!DOCTYPE html><html><body>
   |]
   ++ inner
   ++ [stringQuote|
      </body></html>
   |]

evalHtml :: SocketedOptions -> String
evalHtml (SocketedOptions r p h) = wrapHtml $ socketedScript
   (showWSHost h p)
   r
   [stringQuote|
      function(event) {
         eval(event.data);
      }
   |]
   [stringQuote|
      function(event, error) {
         var c = document.createElement('div');
         c.textContent = 'failed to eval: '+ event.data;
         window.document.body.appendChild(c);
      }
   |]
