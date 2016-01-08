module Main where

import Prelude (Unit, pure, bind, otherwise, top, (>>=), (<$>), (<<<), (<>), ($), (-), (||), (==))
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Printer (printJson)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldr, for_)
import Data.StrMap.ST as STMap
import Data.Maybe (Maybe(..), isJust)
import Data.Set as Set
import Data.String as Str
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console as Console
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.ST (ST)
import Control.Apply ((*>))
import Node.Process (PROCESS)
import Node.Process as Process
import Node.ChildProcess (CHILD_PROCESS)
import Node.ChildProcess as Child
import Node.Stream as Stream
import Node.Buffer (BUFFER)
import Node.Buffer as Buffer
import Node.Encoding as Encoding
import Node.FS (FS)
import Node.FS.Sync as File
import Psa (PsaOptions, parsePsaResult, output)
import Psa.Printer.Default as DefaultPrinter

foreign import version :: String

usage :: String
usage = """psa - Error/Warning reporting frontend for psc 

Usage: psa [--censor-lib] [--censor-src]
           [--censor-codes=CODES] [--filter-codes=CODES]
           [--no-colors] [--no-source]
           [--lib-dir=DIR] [--psc=PSC]
           PSC_OPTIONS

Available options:
  -v,--version           Show the version number
  -h,--help              Show this help text
  --censor-warnings      Censor all warnings
  --censor-lib           Censor warnings from library sources
  --censor-src           Censor warnings from project sources
  --censor-codes=CODES   Censor specific error codes
  --filter-codes=CODES   Only show specific error codes
  --no-colors            Disable ANSI colors
  --no-source            Disable original source code printing
  --lib-dir=DIR          Distinguishing key for lib sources (defaults to 'bower_components')
  --psc=PSC              Name of psc executable (defaults to 'psc')

  CODES                  Comma-separated list of psc error codes
  PSC_OPTIONS            Any extra options are passed to psc
"""

defaultOptions :: PsaOptions
defaultOptions =
  { ansi: true
  , censorWarnings: false
  , censorLib: false
  , censorSrc: false
  , censorCodes: Set.empty
  , filterCodes: Set.empty
  , libDir: "bower_components"
  , cwd: ""
  }

type ParseOptions =
  { extra :: Array String
  , opts :: PsaOptions
  , psc :: String
  , showSource :: Boolean
  }

parseOptions
  :: forall eff
   . PsaOptions
  -> Array String
  -> Eff (console :: CONSOLE, process :: PROCESS | eff) ParseOptions
parseOptions opts =
  Array.foldM parse
    { extra: []
    , psc: "psc"
    , showSource: true
    , opts
    }
  where
  parse p arg
    | arg == "--version" || arg == "-v" =
      Console.log version *> Process.exit 0

    | arg == "--help" || arg == "-h" =
      Console.log usage *> Process.exit 0

    | arg == "--no-source" =
      pure p { showSource = false }

    | arg == "--no-colors" =
      pure p { opts = p.opts { ansi = false } }

    | arg == "--censor-warnings" =
      pure p { opts = p.opts { censorWarnings = true } }

    | arg == "--censor-lib" =
      pure p { opts = p.opts { censorLib = true } }

    | arg == "--censor-lib" =
      pure p { opts = p.opts { censorLib = true } }

    | arg == "--censor-src" =
      pure p { opts = p.opts { censorSrc = true } }

    | isPrefix "--censor-codes=" arg =
      pure p { opts = p.opts { censorCodes = foldr Set.insert p.opts.censorCodes (Str.split "," (Str.drop 15 arg)) } }

    | isPrefix "--filter-codes=" arg =
      pure p { opts = p.opts { filterCodes = foldr Set.insert p.opts.filterCodes (Str.split "," (Str.drop 15 arg)) } }

    | isPrefix "--lib-dir=" arg =
      pure p { opts = p.opts { libDir = Str.drop 10 arg } }

    | isPrefix "--lib-dir=" arg =
      pure p { opts = p.opts { libDir = Str.drop 10 arg } }

    | isPrefix "--psc=" arg =
      pure p { psc = Str.drop 6 arg }

    | otherwise = pure p { extra = Array.snoc p.extra arg }

  isPrefix s str =
    case Str.indexOf s str of
      Just x | x == 0 -> true
      _               -> false

type MainEff h eff =
  ( process :: PROCESS
  , console :: CONSOLE
  , buffer :: BUFFER
  , err :: EXCEPTION
  , fs :: FS
  , cp :: CHILD_PROCESS
  , st :: ST h
  | eff
  )

main :: forall h eff. Eff (MainEff h eff) Unit
main = do
  cwd <- Process.cwd
  argv <- Array.drop 2 <$> Process.argv
  files <- STMap.new
  { extra, opts, psc, showSource } <- parseOptions (defaultOptions { cwd = cwd }) argv

  -- We gotta quote everything again for exec
  let command = Str.joinWith " " $ [psc, "--json-errors"] <> (printJson <<< encodeJson <$> extra)
      execOpts = Child.defaultExecOptions { maxBuffer = Just top }

  Child.exec command execOpts \result -> do
    stderr <- Str.split "\n" <$> Buffer.toString Encoding.UTF8 result.stderr
    for_ stderr \err ->
      case jsonParser err >>= decodeJson >>= parsePsaResult of
        Left _ -> Console.error err
        Right out -> do
          out' <- output (if showSource then loadLines files else loadNothing) opts out
          DefaultPrinter.print opts out'

    if isJust result.error
      then Process.exit 1
      else Process.exit 0

  where
  loadNothing _ _ = pure Nothing

  -- TODO: Handle exceptions
  loadLines files filename pos = do
    contents <- STMap.peek files filename >>= \cache ->
      case cache of
        Just lines -> pure lines
        Nothing -> do
          lines <- Str.split "\n" <$> File.readTextFile Encoding.UTF8 filename
          STMap.poke files filename lines
          pure lines
    pure $ Just $ Array.slice (pos.startLine - 1) (pos.endLine) contents
