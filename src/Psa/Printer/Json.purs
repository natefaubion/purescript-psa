module Psa.Printer.Json
  ( print
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console as Console
import Data.Argonaut.Printer (printJson)
import Psa.Output (Output)
import Psa.Types (encodePsaResult)

print :: forall eff. Output -> Eff (console :: Console.CONSOLE | eff) Unit
print output = do
  let result = encodePsaResult
        { warnings: _.error <$> output.warnings
        , errors: _.error <$> output.errors
        }

  Console.error (printJson result)
