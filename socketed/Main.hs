module Main where

import Data.Semigroup ((<>))
import qualified Options.Applicative as Opt

import Network.Socketed (SocketedOptions(..), runSocketedServer)

params :: Opt.Parser SocketedOptions
params = SocketedOptions
   <$> Opt.option Opt.auto
      ( Opt.long "wait"
      <> Opt.short 'w'
      <> Opt.help "time in milliseconds to wait for replayed inputs"
      <> Opt.showDefault
      <> Opt.value 500
      <> Opt.metavar "INT" )
   <*> Opt.option Opt.auto
      ( Opt.long "port"
      <> Opt.short 'p'
      <> Opt.help "port number the server will be running on"
      <> Opt.showDefault
      <> Opt.value 3000
      <> Opt.metavar "INT" )
   <*> Opt.strOption
      ( Opt.long "bind"
      <> Opt.short 'b'
      <> Opt.help "host ip the server will be bind to"
      <> Opt.showDefault
      <> Opt.value "0.0.0.0"
      <> Opt.metavar "STRING" )

main :: IO ()
main = Opt.execParser opts >>= runSocketedServer
   where
      opts = Opt.info (Opt.helper <*> params)
         (
            Opt.fullDesc
            <> Opt.progDesc "socketed"
            <> Opt.header "socketed"
         )
