module Psa.Printer.Default
  ( renderWarning
  , renderError
  , renderStats
  , renderVerboseStats
  , print
  ) where

import Prelude
import Data.Array as Array
import Data.Foldable (sum, maximum)
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import Data.String as Str
import Data.StrMap as StrMap
import Data.Tuple (Tuple(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console as Console
import Ansi.Output (foreground, dim)
import Ansi.Codes as Ansi
import Psa.Types (Lines, Position, PsaAnnotedError, PsaOptions, PsaPath(..))
import Psa.Output (OutputStats, Output)
import Psa.Printer (Rendered, AnsiText, ansiLength, renderSource, plain, style, indent, line, para, render)
import Psa.Util (replicate, iter_)

-- | Prints output to the console.
print :: forall eff. PsaOptions -> Output -> Eff (console :: Console.CONSOLE | eff) Unit
print options output = do
  iter_ output.warnings \i warning -> do
    Console.error $ toString (renderWarning lenWarnings (i + 1) warning)
    Console.error ""

  iter_ output.errors \i error -> do
    Console.error $ toString (renderError lenErrors (i + 1) error)
    Console.error ""

  Console.error $ toString (renderStats' output.stats)

  where
  toString = render options.ansi
  lenWarnings = Array.length output.warnings
  lenErrors = Array.length output.errors
  renderStats' = if options.verboseStats then renderVerboseStats else renderStats

renderWarning :: Int -> Int -> PsaAnnotedError -> Rendered
renderWarning = renderWrapper (foreground Ansi.Yellow)

renderError :: Int -> Int -> PsaAnnotedError -> Rendered
renderError = renderWrapper (foreground Ansi.Red)

renderWrapper :: Array Ansi.GraphicsParam -> Int -> Int -> PsaAnnotedError -> Rendered
renderWrapper gfx total index { error, path, position, source, message } =
  para
    [ line $
        [ renderStatus gfx total index error.errorCode
        , plain " "
        , renderPath path
        ] <> fromMaybe mempty (renderPosition <$> position)
    , emptyLine
    , indented
         $ fromMaybe mempty (renderSource' <$> position <*> source)
        <> toLines message
    ]

toLines :: String -> Rendered
toLines = para <<< map (line <<< Array.singleton <<< plain) <<< Str.split "\n"

emptyLine :: Rendered
emptyLine = line [ plain "" ]

indented :: Rendered -> Rendered
indented = indent [ plain "  " ]

renderStatus :: Array Ansi.GraphicsParam -> Int -> Int -> String -> AnsiText
renderStatus gfx total index code =
  style gfx $ "[" <> show index <> "/" <> show total <> " " <> code <> "]"

renderPath :: PsaPath -> AnsiText
renderPath (Src f) = plain f
renderPath (Lib f) = plain f
renderPath _       = plain ""

renderPosition :: Position -> Array AnsiText
renderPosition pos =
  [ style dim ":"
  , plain (show pos.startLine)
  , style dim ":"
  , plain (show pos.startColumn)
  ]

renderSource' :: Position -> Lines -> Rendered
renderSource' pos lines = renderSource pos lines <> emptyLine

renderStats :: OutputStats -> Rendered
renderStats stats =
  renderStatCols
    [ [ style (foreground Ansi.Yellow) "Warnings" ]
    , [ style (foreground Ansi.Red) "Errors" ]
    ]
    [ renderStat srcWarnings
    , renderStat srcErrors
    ]
    [ renderStat libWarnings
    , renderStat libErrors
    ]
    [ renderStat allWarnings
    , renderStat allErrors
    ]
  where
  sumRatio (Tuple a b) _ (Tuple c d) = Tuple (a + c) (b + d)
  srcWarnings = StrMap.fold sumRatio (Tuple 0 0) stats.srcWarnings
  srcErrors   = StrMap.fold sumRatio (Tuple 0 0) stats.srcErrors
  libWarnings = StrMap.fold sumRatio (Tuple 0 0) stats.libWarnings
  libErrors   = StrMap.fold sumRatio (Tuple 0 0) stats.libErrors
  allWarnings = StrMap.fold sumRatio (Tuple 0 0) stats.allWarnings
  allErrors   = StrMap.fold sumRatio (Tuple 0 0) stats.allErrors

renderVerboseStats :: OutputStats -> Rendered
renderVerboseStats stats =
  renderStatCols
    (warningLabels <> errorLabels)
    (srcWarnings <> srcErrors)
    (libWarnings <> libErrors)
    (allWarnings <> allErrors)
  where
  warnings = Array.sort (StrMap.keys stats.allWarnings)
  errors   = Array.sort (StrMap.keys stats.allErrors)

  warningLabels = Array.singleton <<< style (foreground Ansi.Yellow) <$> warnings
  errorLabels   = Array.singleton <<< style (foreground Ansi.Red) <$> errors

  getStat key x = fromMaybe (Tuple 0 0) $ StrMap.lookup key x
  getStats ks x = (\k -> renderStat $ getStat k x) <$> ks

  srcWarnings = getStats warnings stats.srcWarnings
  srcErrors   = getStats errors   stats.srcErrors
  libWarnings = getStats warnings stats.libWarnings
  libErrors   = getStats errors   stats.libErrors
  allWarnings = getStats warnings stats.allWarnings
  allErrors   = getStats errors   stats.allErrors

renderStatCols :: Array (Array AnsiText) -> Array (Array AnsiText) -> Array (Array AnsiText) -> Array (Array AnsiText) -> Rendered
renderStatCols col1 col2 col3 col4 = para $
  catRow `map`
    alignLeft ([[ plain "" ]] <> col1)
  `Array.zipWith ($)`
    alignLeft ([[ plain "Src" ]] <> col2)
  `Array.zipWith ($)`
    alignLeft ([[ plain "Lib" ]] <> col3)
  `Array.zipWith ($)`
    alignLeft ([[ plain "All" ]] <> col4)

  where
  gutter = [ plain "   " ]
  catRow c1 c2 c3 c4 =
    line $ c1 <> gutter <> c2 <> gutter <> c3 <> gutter <> c4

  lineLength = sum <<< map ansiLength
  alignRight = align \a as -> Array.cons a as
  alignLeft  = align \a as -> Array.snoc as a
  align f ls = map pad ls where
    max = fromMaybe 0 $ maximum (map lineLength ls)
    pad l = f (plain $ replicate (max - lineLength l) " ") l

renderStat :: Tuple Int Int -> Array AnsiText
renderStat (Tuple 0 0) = [ style (foreground Ansi.Green) "0" ]
renderStat (Tuple a b) | a == b = [ plain $ show a ]
renderStat (Tuple a b) =
  [ plain $ show a
  , style dim $ "/" <> show b
  ]
