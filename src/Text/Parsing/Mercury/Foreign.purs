-- | Foreign imports for Mercury Parser.
module Text.Parsing.Mercury.Foreign where

import Control.Promise (Promise)
import Data.Argonaut.Core (Json)
import Effect (Effect)

-- | Wrappers for this function are provided in `Text.Parsing.Mercury`.
foreign import parse         :: String -> Json -> Effect (Promise Json)

-- | No wrappers are provided for this function
foreign import fetchResource :: String -> Effect (Promise Json)

-- | No wrappers are provided for this function
foreign import addExtractor  :: Json -> Json
