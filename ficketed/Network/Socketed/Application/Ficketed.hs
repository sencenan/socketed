{-# LANGUAGE OverloadedStrings #-}

module Network.Socketed.Application.Ficketed (
      FicketedOptions(..), runFicketedServer, mimeLookup
   ) where

import Data.ByteString (ByteString, isPrefixOf)
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString.Lazy (toStrict)
import Data.Binary.Builder (
      fromByteString, toLazyByteString, putStringUtf8
   )
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Maybe (fromMaybe)
import Data.Map.Strict (insert)
import Data.String.Utils (split)

import Network.Mime (
      FileName, MimeType,
      defaultMimeLookup, defaultMimeMap, mimeByExt
   )
import Network.HTTP.Types (Header)

import Network.Wai (
      Middleware, Response, StreamingBody,
      mapResponseHeaders, responseToStream, responseStream, rawPathInfo
   )
import Network.Wai.Application.Static (
      defaultFileServerSettings, staticApp, ssIndices
   )
import Network.Wai.Handler.Warp (run)

import System.Directory (getCurrentDirectory)

import Text.Blaze.Html5 (text)
import Text.Blaze.Html.Renderer.String (renderHtml)

import Network.Socketed.Application.Ficketed.Internal (FicketedOptions(..))
import Network.Socketed.Application.Ficketed.Template (refreshHtml)

coerceHeaders :: Response -> Response
coerceHeaders = mapResponseHeaders (map f)
   where
      f :: Header -> Header
      f ("Content-Type", _) = ("Content-Type", "text/html")
      f h = h

rewriteHTML :: FicketedOptions -> ByteString -> StreamingBody -> StreamingBody
rewriteHTML opts path sbody send flush
   = sbody send flush
   >> send (fromByteString . pack . refreshHtml (unpack path) $ opts)
   >> flush

rewriteText :: FicketedOptions -> ByteString -> StreamingBody -> StreamingBody
rewriteText opts path sbody send flush
   = send (fromByteString "<pre>")
   >> sbody
      (send
         . putStringUtf8
         . renderHtml
         . text
         . decodeUtf8
         . toStrict
         . toLazyByteString)
      flush
   >> send (fromByteString "</pre>")
   >> send (fromByteString . pack .refreshHtml (unpack path) $ opts)
   >> flush

mimeLookup :: FicketedOptions -> FileName -> MimeType
mimeLookup opts fn = mimeByExt mimeMap (defaultMimeLookup fn) fn
   where
      f m e = insert (T.pack e) "text/plain" m
      mimeMap = foldl f defaultMimeMap $ split "," $ textExts opts

shouldRewrite :: FicketedOptions -> MimeType -> Bool
shouldRewrite (FicketedOptions _ _ _ _ _ x e _ _) m =
   ("text/" `isPrefixOf` m || m `elem` extra)
   && unpack m `notElem` skipped
      where
         skipped = split "," x
         extra = map pack $ split "," e

wrap :: FicketedOptions -> Middleware
wrap opts app req respond = app req f where
   m = mimeLookup opts . decodeUtf8 . rawPathInfo $ req
   f res =
      case () of
      _
         | shouldRewrite opts m ->
            let
               (s, hs, wb) = responseToStream . coerceHeaders $ res
               hMimes = map pack $ split "," (htmlMimes opts)
               rewrite = if m `elem` hMimes then rewriteHTML else rewriteText
            in
               wb
                  $ respond
                  . responseStream s hs
                  . rewrite opts (rawPathInfo req)
         | otherwise -> respond res

runFicketedServer :: FicketedOptions -> IO ()
runFicketedServer opts@(FicketedOptions d p _ _ _ _ _ _ _) = do
   pwd <- getCurrentDirectory
   run p
      $ wrap opts
      $ staticApp (defaultFileServerSettings $ fromMaybe pwd d) {
            ssIndices = [] -- disable auto index
         }
