module Main where

import Data.Semigroup ((<>))

import qualified Options.Applicative as Opt

import Network.Socketed.Internal (stdinPassthrough)
import Network.Socketed.Application.Plocketed.Template (plotHtml)
import Network.Socketed.Application.Plocketed (PlocketedOptions(..))

import System.IO (hFlush, stdout)

params :: Opt.Parser PlocketedOptions
params = PlocketedOptions
   <$> Opt.option Opt.auto
      ( Opt.long "wsport"
      <> Opt.short 'w'
      <> Opt.help "port of the socketed server"
      <> Opt.showDefault
      <> Opt.value 3000
      <> Opt.metavar "INT" )
   <*> Opt.strOption
      ( Opt.long "wsbind"
      <> Opt.short 's'
      <> Opt.help "host of the socketed server"
      <> Opt.showDefault
      <> Opt.value "0.0.0.0"
      <> Opt.metavar "STRING")

main :: IO ()
main = Opt.execParser opts >>= \x -> do
      let s = plotHtml x
      putStrLn s
      putStrLn "\n" -- kick off socketed
      hFlush stdout
      stdinPassthrough
   where
      opts = Opt.info (Opt.helper <*> params)
         (
            Opt.fullDesc
            <> Opt.progDesc "plocketed"
            <> Opt.header "plocketed"
         )
