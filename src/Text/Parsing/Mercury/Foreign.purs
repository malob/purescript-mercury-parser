module Text.Parsing.Mercury.Foreign where

import Control.Promise (Promise)
import Data.Argonaut.Core (Json)
import Effect (Effect)

foreign import parse         :: String -> Json -> Effect (Promise Json)
foreign import fetchResource :: String -> Effect (Promise Json)
foreign import addExtractor  :: Json -> Json
