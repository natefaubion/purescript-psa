module Psa.Printer.Default
  ( renderWarning
  , renderError
  , renderStats
  , print
  ) where

import Prelude (Unit, ($), (+), (-), (<>), show, (==), (<<<), map, (<*>), (<$>), bind)
import Data.Array as Array
import Data.Foldable (sum, maximum)
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import Data.String as Str
import Data.String.Regex as Regex
import Data.Tuple (Tuple(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console as Console
import Ansi.Output (foreground)
import Ansi.Codes as Ansi
import Psa.Types (Lines, Position, PsaAnnotedError, PsaOptions, PsaPath(..))
import Psa.Output (OutputStats, Output)
import Psa.Printer (Rendered, AnsiText, ansiLength, renderSource, plain, style, indent, line, para, renderAnsi, renderRow)
import Psa.Util (replicate, iter_)

-- | Prints output to the console. Errors and warnings will be directed to
-- | stderr, while statistics will be directed to stdout.
print :: forall eff. PsaOptions -> Output -> Eff (console :: Console.CONSOLE | eff) Unit
print options output = do
  iter_ output.warnings \i warning -> do
    Console.error $ toString (renderWarning lenWarnings (i + 1) warning)
    Console.error ""

  iter_ output.errors \i error -> do
    Console.error $ toString (renderError lenErrors (i + 1) error)
    Console.error ""

  Console.log $ toString (renderStats output.stats)

  where
  lenWarnings = Array.length output.warnings
  lenErrors = Array.length output.errors
  toString = renderRow (Str.joinWith "" <<< map (renderAnsi options.ansi))

renderWarning :: Int -> Int -> PsaAnnotedError -> Rendered
renderWarning = render (foreground Ansi.Yellow)

renderError :: Int -> Int -> PsaAnnotedError -> Rendered
renderError = render (foreground Ansi.Red)

render :: Array Ansi.GraphicsParam -> Int -> Int -> PsaAnnotedError -> Rendered
render gfx total index { error, path, position, source } =
  para
    [ line $
        [ renderStatus gfx total index error.errorCode
        , plain " "
        , renderPath path
        ] <> fromMaybe mempty (renderPosition <$> position)
    , emptyLine
    , indented
         $ fromMaybe mempty (renderSource' <$> position <*> source)
        <> toLines error.message
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
  [ style (foreground Ansi.Grey) ":"
  , plain (show pos.startLine)
  , style (foreground Ansi.Grey) ":"
  , plain (show pos.startColumn)
  ]

renderSource' :: Position -> Lines -> Rendered
renderSource' pos lines = renderSource pos lines <> emptyLine

renderStats :: OutputStats -> Rendered
renderStats stats = para $
  catRow `map`
    alignLeft
      [ [ plain "" ]
      , [ style (foreground Ansi.Yellow) "Warnings" ]
      , [ style (foreground Ansi.Red) "Errors" ]

      ]
  `Array.zipWith ($)`
    alignLeft
      [ [ plain "Src" ]
      , renderStat stats.srcWarnings
      , renderStat stats.srcErrors
      ]
  `Array.zipWith ($)`
    alignLeft
      [ [ plain "Lib" ]
      , renderStat stats.libWarnings
      , renderStat stats.libErrors
      ]
  `Array.zipWith ($)`
    alignLeft
      [ [ plain "All" ]
      , renderStat stats.allWarnings
      , renderStat stats.allErrors
      ]
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
  , style (foreground Ansi.Grey) $ "/" <> show b
  ]
