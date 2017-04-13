module Main where

import Data.Semigroup ((<>))

import qualified Options.Applicative as Opt

import Network.Socketed.Internal (stdinPassthrough)
import Network.Socketed.Application.Plocketed.Template (plotHtml)
import Network.Socketed.Application.Plocketed.Internal (PlocketedOptions(..))

import System.IO (hFlush, stdout)

params :: Opt.Parser PlocketedOptions
params = PlocketedOptions
   <$> Opt.option Opt.auto
      ( Opt.long "width"
      <> Opt.short 'x'
      <> Opt.help "width of the chart"
      <> Opt.showDefault
      <> Opt.value 1100
      <> Opt.metavar "INT" )
   <*> Opt.option Opt.auto
      ( Opt.long "height"
      <> Opt.short 'y'
      <> Opt.help "height of the chart"
      <> Opt.showDefault
      <> Opt.value 600
      <> Opt.metavar "INT")
   <*> Opt.option Opt.auto
      ( Opt.long "maxData"
      <> Opt.short 'm'
      <> Opt.help "max data point kept"
      <> Opt.showDefault
      <> Opt.value 20
      <> Opt.metavar "INT")

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
