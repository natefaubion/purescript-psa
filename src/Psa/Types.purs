module Psa.Types
  ( ErrorCode
  , ModuleName
  , Filename
  , PsaOptions
  , PsaResult
  , PsaError
  , PsaAnnotedError
  , PsaPath(..)
  , Position
  , Suggestion
  , Lines
  , parsePsaResult
  , parsePsaError
  , encodePsaResult
  , encodePsaError
  , compareByLocation
  ) where

import Prelude
import Data.Argonaut.Core (Json, JObject, jsonNull)
import Data.Argonaut.Decode (decodeJson, class DecodeJson)
import Data.Argonaut.Decode.Combinators ((.?))
import Data.Argonaut.Encode (encodeJson)
import Data.StrMap as StrMap
import Data.StrMap.ST as STMap
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Control.Monad.Eff (runPure)
import Unsafe.Coerce (unsafeCoerce)

type ErrorCode = String
type ModuleName = String
type Filename = String
type Lines = Array String

-- | Relative files paths from the cwd, tagged as either being part of the
-- | source files or library files of a project. The `Unknown` variant exists
-- | because some psc errors are inter-module and aren't reported with a
-- | canonical file.
data PsaPath
  = Src String
  | Lib String
  | Unknown

instance eqPsaPath :: Eq PsaPath where
  eq (Src a) (Src b) = eq a b
  eq (Lib a) (Lib b) = eq a b
  eq Unknown Unknown = true
  eq _       _       = false

instance ordPsaPath :: Ord PsaPath where
  compare (Src a) (Src b) = compare a b
  compare (Src _) (Lib _) = GT
  compare (Src _) Unknown = GT
  compare (Lib _) (Src _) = LT
  compare (Lib a) (Lib b) = compare a b
  compare (Lib _) Unknown = GT
  compare Unknown Unknown = EQ
  compare Unknown _       = LT

type PsaOptions =
  { ansi :: Boolean
  , censorWarnings :: Boolean
  , censorLib :: Boolean
  , censorSrc :: Boolean
  , censorCodes :: Set ErrorCode
  , filterCodes :: Set ErrorCode
  , verboseStats :: Boolean
  , libDirs :: Array String
  , strict :: Boolean
  , cwd :: String
  }

type PsaResult =
  { warnings :: Array PsaError
  , errors :: Array PsaError
  }

type PsaError =
  { moduleName :: Maybe ModuleName
  , errorCode :: ErrorCode
  , errorLink :: String
  , suggestion :: Maybe Suggestion
  , message :: String
  , filename :: Maybe Filename
  , position :: Maybe Position
  }

type PsaAnnotedError =
  { error :: PsaError
  , path :: PsaPath
  , source :: Maybe Lines
  , position :: Maybe Position
  , message :: String
  }

type Position =
  { startLine :: Int
  , startColumn :: Int
  , endLine :: Int
  , endColumn :: Int
  }

type Suggestion =
  { replacement :: String
  , replaceRange :: Maybe Position
  }

compareByLocation :: PsaAnnotedError -> PsaAnnotedError -> Ordering
compareByLocation err1 err2 =
  case compare err1.path err2.path of
    EQ ->
      case err1.position, err2.position of
        Nothing, Nothing -> EQ
        Nothing, _       -> LT
        _      , Nothing -> GT
        Just a , Just b  ->
          compare (Tuple a.startLine a.startColumn)
                  (Tuple b.startLine b.startColumn)
    x  -> x

parsePsaResult :: JObject -> Either String PsaResult
parsePsaResult obj =
  { warnings: _
  , errors: _
  } <$> (obj .? "warnings" >>= traverse parsePsaError)
    <*> (obj .? "errors" >>= traverse parsePsaError)

parsePsaError :: JObject -> Either String PsaError
parsePsaError obj =
  { moduleName: _
  , errorCode: _
  , errorLink: _
  , message: _
  , filename: _
  , position: _
  , suggestion: _
  } <$> obj .? "moduleName"
    <*> obj .? "errorCode"
    <*> obj .? "errorLink"
    <*> obj .? "message"
    <*> obj .? "filename"
    <*> (obj .? "position" >>= parsePosition)
    <*> (obj .? "suggestion" >>= parseSuggestion)

parsePosition :: Maybe JObject -> Either String (Maybe Position)
parsePosition =
  maybe (pure Nothing) \obj -> map Just $
    { startLine: _
    , startColumn: _
    , endLine: _
    , endColumn: _
    } <$> obj .? "startLine"
      <*> obj .? "startColumn"
      <*> obj .? "endLine"
      <*> obj .? "endColumn"

parseSuggestion :: Maybe JObject -> Either String (Maybe Suggestion)
parseSuggestion =
  maybe (pure Nothing) \obj -> map Just $
    { replacement: _
    , replaceRange: _
    } <$> obj .? "replacement"
      <*> (obj .?? "replaceRange" >>= parsePosition)

encodePsaResult :: PsaResult -> Json
encodePsaResult res = encodeJson $ runPure $ StrMap.runST do
  obj <- STMap.new
  STMap.poke obj "warnings" $ encodeJson (encodePsaError <$> res.warnings)
  STMap.poke obj "errors"   $ encodeJson (encodePsaError <$> res.errors)

encodePsaError :: PsaError -> Json
encodePsaError error = encodeJson $ runPure $ StrMap.runST do
  obj <- STMap.new
  STMap.poke obj "moduleName"  $ encodeJson error.moduleName
  STMap.poke obj "errorCode"   $ encodeJson error.errorCode
  STMap.poke obj "errorLink"   $ encodeJson error.errorLink
  STMap.poke obj "message"     $ encodeJson error.message
  STMap.poke obj "filename"    $ encodeJson error.filename
  STMap.poke obj "position"    $ encodeJson (maybe jsonNull encodePosition error.position)
  STMap.poke obj "suggestion"  $ encodeJson (maybe jsonNull encodeSuggestion error.suggestion)

encodePosition :: Position -> Json
encodePosition = unsafeCoerce

encodeSuggestion :: Suggestion -> Json
encodeSuggestion suggestion = encodeJson $ runPure $ StrMap.runST do
  obj <- STMap.new
  STMap.poke obj "replacement"  $ encodeJson suggestion.replacement
  STMap.poke obj "replaceRange" $ encodeJson (maybe jsonNull encodePosition suggestion.replaceRange)

maybeProp :: forall a. (DecodeJson a) => JObject -> String -> Either String (Maybe a)
maybeProp obj key = maybe (Right Nothing) decodeJson (StrMap.lookup key obj)

infix 7 maybeProp as .??
