{-# LANGUAGE OverloadedStrings #-}

module Main where

import Conduit (
      ConduitM, MonadIO,
      (.|), awaitForever, mapC, mapM_C, runConduitRes, liftIO, mapMC,
      sourceHandle, sourceFileBS, stdoutC, runResourceT
   )
import qualified Data.Conduit.Binary as CB

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (append, unpack)
-- import Data.Text.Lazy (pack, unpack)

import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Markdown (def, markdown, msXssProtect)

import System.IO (stdin, stdout)

-- render str = renderHtml $ markdown def { msXssPrxotect = False } (
--       pack $ str ++ "<script>console.log('wha wha??')</script>"
--    )

-- render = withFile file ReadMode renderHtml $ markdown def $ hGetContents

main :: IO ()
main = runConduitRes
   $ sourceHandle stdin
   .| CB.lines
   .| mapC unpack
   .| awaitForever sourceFileBS
   .| CB.lines
   .| mapC (flip append "\n")
   .| mapM_C (liftIO . print)
   -- .| mapM_C (\x -> (liftIO . print) x)
   -- .| mapM_C (\x -> runResourceT $ (liftIO . print) x)
   -- .| stdoutC
