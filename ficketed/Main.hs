module Main where

import Control.Applicative (optional)

import Data.Semigroup ((<>))

import qualified Options.Applicative as Opt

import Network.Socketed.Internal (withStdinPassthrough)
import Network.Socketed.Application.Ficketed (
      FicketedOptions(..), runFicketedServer
   )

params :: Opt.Parser FicketedOptions
params = FicketedOptions
   <$> optional (
         Opt.argument Opt.str
         ( Opt.help "directory to host"
         <> Opt.metavar "PATH")
      )
   <*> Opt.option Opt.auto
      ( Opt.long "port"
      <> Opt.short 'p'
      <> Opt.help "port number the server will be running on"
      <> Opt.showDefault
      <> Opt.value 3001
      <> Opt.metavar "INT" )
   <*> Opt.strOption
      ( Opt.long "bind"
      <> Opt.short 'b'
      <> Opt.help "host ip the server will be bind to"
      <> Opt.showDefault
      <> Opt.value "0.0.0.0"
      <> Opt.metavar "STRING" )
   <*> Opt.option Opt.auto
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
   <*> Opt.strOption
      ( Opt.long "skipedMimes"
      <> Opt.short 'x'
      <> Opt.help "skip update script for mime types listed in this list"
      <> Opt.showDefault
      <> Opt.value "text/css"
      <> Opt.metavar "COMMA SEPERATED LIST")
   <*> Opt.strOption
      ( Opt.long "extraMimes"
      <> Opt.short 'e'
      <> Opt.help "additional mime types to be subject for refreshing"
      <> Opt.showDefault
      <> Opt.value "application/json,application/x-sh"
      <> Opt.metavar "COMMA SEPERATED LIST")
   <*> Opt.strOption
      ( Opt.long "htmlMimes"
      <> Opt.short 'l'
      <> Opt.help "mime types that will not have escape html performed"
      <> Opt.showDefault
      <> Opt.value "text/html,text/javascipt"
      <> Opt.metavar "COMMA SEPERATED LIST")
   <*> Opt.strOption
      ( Opt.long "textExts"
      <> Opt.short 't'
      <> Opt.help "additional file extensions to be treated as text/plain"
      <> Opt.showDefault
      <> Opt.value "md,yaml"
      <> Opt.metavar "COMMA SEPERATED LIST")

main :: IO ()
main =  Opt.execParser opts >>= withStdinPassthrough . runFicketedServer
   where
      opts = Opt.info (Opt.helper <*> params)
         (
            Opt.fullDesc
            <> Opt.progDesc "ficketed"
            <> Opt.header "ficketed"
         )
