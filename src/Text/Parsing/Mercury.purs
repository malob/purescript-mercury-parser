module Text.Parsing.Mercury
  ( module Text.Parsing.Mercury.Types
  , module Text.Parsing.Mercury
  ) where

import Prelude

import Control.Promise (toAffE)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Data.Map (empty)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, error, throwError)

import Text.Parsing.Mercury.Types
import Text.Parsing.Mercury.Foreign as FFI

-- | Run Mercury Parser using `defaultParseOptions`.
parse :: Url -> Aff ParseResponse
parse = parseWithOptions $ _ {html = Nothing}

-- | Run Mercury Parser with some options changed.
-- |
-- | Options are provide using annonymous record update syntax,
-- |
-- | ```purescript
-- | parseWithOptions _ { contentType = Text, fallback = false } "https://example.com"
-- | ```
-- |
-- | which is applied to `defaultParseOptions`.
parseWithOptions :: (ParseOptions -> ParseOptions) -> Url -> Aff ParseResponse
parseWithOptions ops url = do
  res <- toAffE $ FFI.parse url (encodeJson $ ops defaultParseOptions)
  case decodeResponse res of
    Right x -> pure x
    Left  _ -> case decodeError res of
      Right err -> throwError $ error err.message
      Left _    -> throwError $ error "Could not parse output from Mercury Parser"

  where
    decodeResponse = decodeJson :: Json -> Either String ParseResponse
    decodeError    = decodeJson :: Json -> Either String ErrorResponse

-- | Default options used by Mercury Parser:
-- |
-- | ```purescript
-- | { html: Nothing
-- | , fetchAllPages: true
-- | , fallback: true
-- | , contentType: Html
-- | , headers: Headers empty
-- | , customExtractor: Nothing
-- | }
-- | ```
defaultParseOptions :: ParseOptions
defaultParseOptions =
  { html: Nothing
  , fetchAllPages: true
  , fallback: true
  , contentType: Html
  , headers: Headers empty
  --, extend: Nothing
  , customExtractor: Nothing
  }
