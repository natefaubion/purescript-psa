module Main where

import Prelude
import Data.Argonaut.Printer (printJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Array as Array
import Data.DateTime.Instant (toDateTime)
import Data.Either (Either(..))
import Data.Foldable (foldr, for_)
import Data.Traversable (traverse)
import Data.StrMap as StrMap
import Data.StrMap.ST as STMap
import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import Data.String as Str
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console as Console
import Control.Monad.Eff.Exception (EXCEPTION, Error, catchException, throw, throwException)
import Control.Monad.Eff.Now (NOW, now)
import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Node.Platform (Platform(Win32))
import Node.Process (PROCESS)
import Node.Process as Process
import Node.ChildProcess (CHILD_PROCESS)
import Node.ChildProcess as Child
import Node.Stream as Stream
import Node.Buffer (BUFFER)
import Node.Encoding as Encoding
import Node.FS (FS)
import Node.FS.Sync as File
import Node.FS.Stats as Stats
import Node.Path as Path
import Unsafe.Coerce (unsafeCoerce)
import Partial.Unsafe (unsafePartial)
import Psa (PsaOptions, parsePsaResult, parsePsaError, encodePsaError, output)
import Psa.Printer.Default as DefaultPrinter
import Psa.Printer.Json as JsonPrinter

foreign import version :: String

defaultOptions :: PsaOptions
defaultOptions =
  { ansi: true
  , censorWarnings: false
  , censorLib: false
  , censorSrc: false
  , censorCodes: Set.empty
  , filterCodes: Set.empty
  , verboseStats: false
  , libDirs: []
  , strict: false
  , cwd: ""
  }

type ParseOptions =
  { extra :: Array String
  , opts :: PsaOptions
  , purs :: String
  , showSource :: Boolean
  , stash :: Boolean
  , stashFile :: String
  , jsonErrors :: Boolean
  }

parseOptions
  :: forall eff
   . PsaOptions
  -> Array String
  -> Eff (console :: CONSOLE, process :: PROCESS | eff) ParseOptions
parseOptions opts args =
  defaultLibDir <$>
    Array.foldM parse
      { extra: []
      , purs: "purs"
      , showSource: true
      , stash: false
      , stashFile: ".psa-stash"
      , jsonErrors: false
      , opts
      }
      args
  where
  parse p arg
    | arg == "--version" || arg == "-v" =
      Console.log version *> Process.exit 0

    | arg == "--help" || arg == "-h" =
      Console.log usage *> Process.exit 0

    | arg == "--stash" =
      pure p { stash = true }

    | arg == "--json-errors" =
      pure p { jsonErrors = true }

    | arg == "--no-source" =
      pure p { showSource = false }

    | arg == "--no-colors" || arg == "--monochrome" =
      pure p { opts = p.opts { ansi = false } }

    | arg == "--verbose-stats" =
      pure p { opts = p.opts { verboseStats = true } }

    | arg == "--strict" =
      pure p { opts = p.opts { strict = true } }

    | arg == "--censor-warnings" =
      pure p { opts = p.opts { censorWarnings = true } }

    | arg == "--censor-lib" =
      pure p { opts = p.opts { censorLib = true } }

    | arg == "--censor-lib" =
      pure p { opts = p.opts { censorLib = true } }

    | arg == "--censor-src" =
      pure p { opts = p.opts { censorSrc = true } }

    | isPrefix "--censor-codes=" arg =
      pure p { opts = p.opts { censorCodes = foldr Set.insert p.opts.censorCodes (Str.split (Str.Pattern ",") (Str.drop 15 arg)) } }

    | isPrefix "--filter-codes=" arg =
      pure p { opts = p.opts { filterCodes = foldr Set.insert p.opts.filterCodes (Str.split (Str.Pattern ",") (Str.drop 15 arg)) } }

    | isPrefix "--is-lib=" arg =
      pure p { opts = p.opts { libDirs = Array.snoc p.opts.libDirs (Str.drop 9 arg) } }

    | isPrefix "--purs=" arg =
      pure p { purs = Str.drop 7 arg }

    | isPrefix "--stash=" arg =
      pure p { stash = true, stashFile = Str.drop 8 arg }

    | otherwise = pure p { extra = Array.snoc p.extra arg }

  isPrefix s str =
    case Str.indexOf (Str.Pattern s) str of
      Just x | x == 0 -> true
      _               -> false

  defaultLibDir x
    | Array.length x.opts.libDirs == 0 =
      x { opts = x.opts { libDirs = [ "bower_components" ] } }
    | otherwise = x

type MainEff h eff =
  ( process :: PROCESS
  , console :: CONSOLE
  , buffer :: BUFFER
  , err :: EXCEPTION
  , fs :: FS
  , cp :: CHILD_PROCESS
  , st :: ST h
  , now :: NOW
  | eff
  )

main :: forall h eff. Eff (MainEff h eff) Unit
main = void do
  cwd <- Process.cwd
  argv <- Array.drop 2 <$> Process.argv

  { extra
  , opts
  , purs
  , showSource
  , stash
  , stashFile
  , jsonErrors
  } <- parseOptions (defaultOptions { cwd = cwd }) argv

  let opts' = opts { libDirs = (_ <> Path.sep) <<< Path.resolve [cwd] <$> opts.libDirs }
      args  = Array.cons "compile" $ Array.cons "--json-errors" extra

  stashData <-
    if stash
      then readStashFile stashFile
      else emptyStash

  spawn' purs args \buffer -> do
    let stderr = Str.split (Str.Pattern "\n") buffer
    for_ stderr \err ->
      case jsonParser err >>= decodeJson >>= parsePsaResult of
        Left _ -> Console.error err
        Right out -> do
          files <- STMap.new
          let loadLinesImpl = if showSource then loadLines files else loadNothing
              filenames = insertFilenames (insertFilenames Set.empty out.errors) out.warnings
          merged <- mergeWarnings filenames stashData.date stashData.stash out.warnings
          when stash $ writeStashFile stashFile merged
          out' <- output loadLinesImpl opts' out { warnings = merged }
          if jsonErrors
            then JsonPrinter.print out'
            else DefaultPrinter.print opts' out'
          if StrMap.isEmpty out'.stats.allErrors
            then Process.exit 0
            else Process.exit 1

  where
  insertFilenames = foldr \x s -> maybe s (flip Set.insert s) x.filename
  stdio = [ Just Child.Pipe, unsafePartial (Array.unsafeIndex Child.inherit 1), Just Child.Pipe ]
  loadNothing _ _ = pure Nothing

  spawn' cmd args onExit = do
    child <- Child.spawn cmd args Child.defaultSpawnOptions { stdio = stdio }
    buffer <- ST.newSTRef ""
    Stream.onDataString (Child.stderr child) Encoding.UTF8 \chunk ->
      void $ ST.modifySTRef buffer (_ <> chunk)
    Child.onExit child \status ->
      case status of
        Child.Normally n -> do
          ST.readSTRef buffer >>= onExit
          Process.exit n
        Child.BySignal s -> do
          Console.error (show s)
          Process.exit 1
    Child.onError child (retryWithCmd cmd args onExit)

  retryWithCmd cmd args onExit err
    | err.code == "ENOENT" = do
     -- On windows, if the executable wasn't found, try adding .cmd
     if Process.platform == Win32
       then
         case Str.stripSuffix (Str.Pattern ".cmd") cmd of
           Nothing      -> spawn' (cmd <> ".cmd") args onExit
           Just bareCmd -> throw $ "`" <> bareCmd <> "` executable not found. (nor `" <> cmd <> "`)"
       else
         throw $ "`" <> cmd <> "` executable not found."
    | otherwise =
       throwException (Child.toStandardError err)

  -- TODO: Handle exceptions
  loadLines files filename pos = do
    contents <- STMap.peek files filename >>= \cache ->
      case cache of
        Just lines -> pure lines
        Nothing -> do
          lines <- Str.split (Str.Pattern "\n") <$> File.readTextFile Encoding.UTF8 filename
          STMap.poke files filename lines
          pure lines
    let source = Array.slice (pos.startLine - 1) (pos.endLine) contents
    pure $ Just source

  decodeStash s = jsonParser s >>= decodeJson >>= traverse parsePsaError
  encodeStash s = encodeJson (encodePsaError <$> s)
  emptyStash    = { date: _ , stash: [] } <$> toDateTime <$> now

  readStashFile stashFile = catchException' (const emptyStash) do
    stat <- File.stat stashFile
    file <- File.readTextFile Encoding.UTF8 stashFile
    case decodeStash file of
      Left _ -> emptyStash
      Right stash -> pure { date: Stats.modifiedTime stat, stash }

  writeStashFile stashFile warnings = do
    let file = printJson (encodeStash warnings)
    File.writeTextFile Encoding.UTF8 stashFile file

  mergeWarnings filenames date old new = do
    fileStat <- STMap.new
    old' <- flip Array.filterM old \x ->
      case x.filename of
        Nothing -> pure false
        Just f ->
          if Set.member f filenames
            then pure false
            else do
              stat <- STMap.peek fileStat f
              case stat of
                Just s -> pure s
                Nothing -> do
                  s <- catchException' (\_ -> pure false) $
                    (date > _) <<< Stats.modifiedTime <$> File.stat f
                  STMap.poke fileStat f s
                  pure s
    pure $ old' <> new

usage :: String
usage = """psa - Error/Warning reporting frontend for 'purs compile'

Usage: psa [--censor-lib] [--censor-src]
           [--censor-codes=CODES] [--filter-codes=CODES]
           [--no-colors] [--no-source]
           [--is-lib=DIR] [--purs=PURS] [--stash]
           PSC_OPTIONS

Available options:
  -v,--version           Show the version number
  -h,--help              Show this help text
  --verbose-stats        Show counts for each warning type
  --censor-warnings      Censor all warnings
  --censor-lib           Censor warnings from library sources
  --censor-src           Censor warnings from project sources
  --censor-codes=CODES   Censor specific error codes
  --filter-codes=CODES   Only show specific error codes
  --no-colors            Disable ANSI colors
  --no-source            Disable original source code printing
  --strict               Promotes src warnings to errors
  --stash                Enable persistent warnings (defaults to .psa-stash)
  --stash=FILE           Enable persistent warnings using a specific stash file
  --is-lib=DIR           Distinguishing library path (defaults to 'bower_components')
  --purs=PURS            Name of purs executable (defaults to 'purs')

  CODES                  Comma-separated list of purs error codes
  PSC_OPTIONS            Any extra options are passed to 'purs compile'
"""

-- Due to `catchException` label annoyingness
catchException'
  :: forall a eff
   . (Error -> Eff (err :: EXCEPTION | eff) a)
  -> Eff (err :: EXCEPTION | eff) a
  -> Eff (err :: EXCEPTION | eff) a
catchException' eb eff = unsafeCoerce (catchException (unsafeCoerce eb) (unsafeCoerce eff))
