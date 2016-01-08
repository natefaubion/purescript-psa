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
  , Lines
  , parsePsaResult
  ) where

import Prelude ((<*>), (<$>), map, ($), pure, (>>=))
import Data.Argonaut.Core (JObject)
import Data.Argonaut.Combinators ((.?))
import Data.Either (Either)
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set)
import Data.Traversable (traverse)

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

type PsaOptions =
  { ansi :: Boolean
  , censorWarnings :: Boolean
  , censorLib :: Boolean
  , censorSrc :: Boolean
  , censorCodes :: Set ErrorCode
  , filterCodes :: Set ErrorCode
  , libDir :: String
  , cwd :: String
  }

type PsaResult =
  { warnings :: Array PsaError
  , errors :: Array PsaError
  }

type PsaError =
  { moduleName :: Maybe ModuleName
  , errorCode :: ErrorCode
  , message :: String
  , filename :: Maybe Filename
  , position :: Maybe Position
  }

type PsaAnnotedError =
  { error :: PsaError
  , path :: PsaPath
  , source :: Maybe Lines
  , position :: Maybe Position
  }

type Position =
  { startLine :: Int
  , startColumn :: Int
  , endLine :: Int
  , endColumn :: Int
  }

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
  , message: _
  , filename: _
  , position: _
  } <$> obj .? "moduleName"
    <*> obj .? "errorCode"
    <*> obj .? "message"
    <*> obj .? "filename"
    <*> (obj .? "position" >>= parsePosition)

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
