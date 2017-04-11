{-# LANGUAGE QuasiQuotes #-}

module Network.Socketed.Template (evalHtml) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)

import Network.Socketed.Internal (SocketedOptions(..), showHost, stringQuote)

evalHtml :: SocketedOptions -> ByteString
evalHtml opts@(SocketedOptions r _ _) = pack $ [stringQuote|
   <!DOCTYPE html><html><body><script>
   (function(host, linesToDrop) {
      window.handleMessage = function(event) {
         eval(event.data);
      };

      window.handleMessageError = function(event, error) {
         var c = document.createElement('div');
         c.textContent = 'failed to eval: '+ event.data;
         window.document.body.appendChild(c);
      };

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
   })(
   |]
   ++ "'" ++ showHost opts ++ "', " ++ show r
   ++ [stringQuote|
   )</script></body></html>
   |]
