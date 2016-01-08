module Psa.Output
  ( output
  , Output
  , OutputStats
  ) where

import Prelude (class Monad, bind, otherwise, not, pure, top, id, (||), (&&), ($), (+), (-), (<), (<=), (==), (<*>), (<$>), (<>), (<<<), (>>>))
import Data.Foldable (foldl)
import Data.Maybe (maybe, Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Set as Set
import Data.String as Str
import Data.Array as Array
import Node.Path as Path
import Psa.Types (PsaOptions, PsaError, PsaAnnotedError, PsaPath(..), PsaResult, Position, Filename, Lines)

data ErrorTag = Error | Warning

type Output =
  { warnings :: Array PsaAnnotedError
  , errors :: Array PsaAnnotedError
  , stats :: OutputStats
  }

-- | Statistics are a ratio of errors shown to errors in total.
type OutputStats =
  { allWarnings :: Tuple Int Int
  , allErrors   :: Tuple Int Int
  , srcWarnings :: Tuple Int Int
  , srcErrors   :: Tuple Int Int
  , libWarnings :: Tuple Int Int
  , libErrors   :: Tuple Int Int
  }

initialStats :: OutputStats
initialStats =
  { allWarnings: Tuple 0 0
  , allErrors:   Tuple 0 0
  , srcWarnings: Tuple 0 0
  , srcErrors:   Tuple 0 0
  , libWarnings: Tuple 0 0
  , libErrors:   Tuple 0 0
  }

-- | Annotates a error/warning result set with original source lines, better
-- | positions, and semantic paths (lib vs src). The callback should load the
-- | requested set of lines from the absolute filename based on the tentative
-- | position information.
output
  :: forall m
   . (Monad m)
  => (Filename -> Position -> m (Maybe Lines))
  -> PsaOptions
  -> PsaResult
  -> m Output
output loadLines options result = do
  let initialState = { warnings: [], errors: [], stats: initialStats }
  state  <- Array.foldM (onError Warning) initialState result.warnings
  state' <- Array.foldM (onError Error) state result.errors
  pure state'

  where
  onError :: ErrorTag -> Output -> PsaError -> m Output
  onError tag state error =
    if shouldShowError options tag path error.errorCode
      then do
        source <- fromMaybe (pure Nothing) (loadLines <$> error.filename <*> error.position)
        let position = trimPosition <$> source <*> error.position
            source' = (\p -> Array.take (p.endLine - p.startLine + 1)) <$> position <*> source
        update [{ error: error { message = trimMessage error.message }, path, position, source: source' }]
      else
        update []

    where
    path :: PsaPath
    path = maybe Unknown (errorPath options.libDir <<< Path.relative options.cwd) error.filename

    update :: Array PsaAnnotedError -> m Output
    update log =
      pure $ onTag
        (_ { stats = stats, errors = state.errors <> log })
        (_ { stats = stats, warnings = state.warnings <> log })
        tag state
      where
      stats = updateStats tag path (not (Array.null log)) state.stats

updateStats
  :: ErrorTag
  -> PsaPath
  -> Boolean -- If the error was printed
  -> OutputStats
  -> OutputStats
updateStats tag path printed s =
  { allWarnings: onTag id bump tag s.allWarnings
  , allErrors:   onTag bump id tag s.allErrors
  , srcWarnings: onTag id (onPath bump id path) tag s.srcWarnings
  , srcErrors:   onTag (onPath bump id path) id tag s.srcErrors
  , libWarnings: onTag id (onPath id bump path) tag s.libWarnings
  , libErrors:   onTag (onPath id bump path) id tag s.libErrors
  }

  where
  bump :: Tuple Int Int -> Tuple Int Int
  bump (Tuple a b) = Tuple (if printed then a + 1 else a) (b + 1)

shouldShowError :: PsaOptions -> ErrorTag -> PsaPath -> String -> Boolean
shouldShowError { filterCodes, censorCodes, censorSrc, censorLib, censorWarnings } tag path code =
  not (censorWarnings && isWarning tag)
  && not (censorSrc && isSrc path || censorLib && isLib path)
  && (Set.isEmpty filterCodes || Set.member code filterCodes)
  && (Set.isEmpty censorCodes || not (Set.member code censorCodes))

errorPath :: String -> String -> PsaPath
errorPath lib s
  | Str.contains lib s = Lib s
  | otherwise          = Src s
  
onTag :: forall a b. (a -> b) -> (a -> b) -> ErrorTag -> a -> b
onTag f g Error   x = f x
onTag f g Warning x = g x

onPath :: forall a. (a -> a) -> (a -> a) -> PsaPath -> a -> a
onPath f g (Src _) x = f x
onPath f g (Lib _) x = g x
onPath f g _       x = x

isLib :: PsaPath -> Boolean
isLib (Lib _) = true
isLib _       = false

isSrc :: PsaPath -> Boolean
isSrc (Src _) = true
isSrc _       = false

isWarning :: ErrorTag -> Boolean
isWarning Warning = true
isWarning _       = false

-- | Finds the true bounds of the source. The PureScript compiler is greedy
-- | when it comes to matching whitespace at the end of an expression, so the
-- | original source bounds always includes whitespace and comments.
trimPosition :: Lines -> Position -> Position
trimPosition lines pos =
  case lines of
    [] ->
      { startLine: pos.startLine
      , startColumn: pos.startColumn
      , endLine: pos.startLine
      , endColumn: pos.startColumn
      }

    [l] ->
      case trimCol (pos.endColumn) l of
        Just col -> pos { endLine = pos.startLine, endColumn = col }
        Nothing  -> pos { endLine = pos.startLine, endColumn = pos.startColumn }

    _ ->
      case trimPos { row: pos.endLine, col: pos.endColumn } of
        Just { row, col } -> pos { endLine = row, endColumn = col }
        Nothing           -> trimPosition [] pos

  where
  -- WARNING: here be off-by-one dragons!

  trimPos { row, col }
    | col <= 1 =
      case Array.index lines (row - pos.startLine - 1) of
        Just l -> trimPos { row: row - 1, col: Str.length l + 1 }
        _      -> Nothing

    | otherwise =
      case Array.index lines (row - pos.startLine) of
        Just l ->
          case trimCol col l of
            Just col' -> Just { row, col: col' }
            Nothing   -> trimPos { row, col: 1 }

        _      -> Nothing

  trimCol col l =
    case Str.charAt (col - 2) l of
      Just x | isPunc x -> trimCol (col - 1) l
      Just _            -> trimComment col l
      _                 -> Nothing

  -- TODO: this breaks if "--" is inside a quoted string.
  -- TODO: Block comments?
  trimComment col l =
    case Str.indexOf "--" l of
      Just x | x == 0        -> Nothing
      Just x | x < (col - 1) -> Just (x + 1)
      _                      -> Just col

  isPunc ' ' = true
  isPunc ',' = true
  isPunc _   = false

-- | Trims extraneous whitespace from psc error messages.
trimMessage :: String -> String
trimMessage =
  Str.split "\n"
  >>> foldl dedent { lines: [], indent: top }
  >>> _.lines
  >>> foldl collapse []
  >>> Str.joinWith "\n"
  >>> Str.trim

  where
  dedent { lines, indent } l
    | l == ""   = { lines: Array.snoc lines l, indent }
    | otherwise =
      let indent' = Str.length $ Str.takeWhile (== ' ') l in
      if indent' < indent
        then { lines: Array.snoc lines (Str.drop indent' l), indent: indent' }
        else { lines: Array.snoc lines (Str.drop indent l), indent }

  collapse lines l =
    case Array.last lines of
      Just "" | l == "" -> lines
      _                 -> Array.snoc lines l
