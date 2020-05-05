-- | A PureScript wrapper for [Mercury Parser](https://github.com/postlight/mercury-parser) v2.2.0.
-- |
-- | Core features works reliably, namely [parse](#v:parse), and
-- | [parseWithOptions](#v:parseWithOptions) with basic options changed.
-- |
-- | This package is still in early development so the API might change in drastic ways.
-- |
-- | See "wrapper implementation notes" in this documentation for information about implementation
-- | details and functionality that has not been implemented or properly tested yet, in particular
-- | on [ParserOptions](#t:ParserOptions), [CustomParser](#t:CustomParser), and
-- | [ContentExtractor](#t:ContentExtractor).
module Text.Parsing.Mercury
  ( parse
  , parseWithOptions
  , defaultOptions
  , module Text.Parsing.Mercury.Types
  ) where

import Prelude
import Text.Parsing.Mercury.Types

import Control.Promise (toAffE)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Data.Map (empty)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, error, throwError)
import Record (rename)
import Text.Parsing.Mercury.Foreign as FFI
import Type.Prelude (SProxy(..))

-- | Run Mercury Parser using [defaultOptions](#v:defaultOptions).
parse :: Url -> Aff ParsedContent
parse = parseWithOptions $ _ {html = Nothing}

-- | Run Mercury Parser with some options changed.
-- |
-- | Options are provide using anonymous record update syntax,
-- |
-- | ```purescript
-- | parseWithOptions
-- |   _ { contentType = Text, fallback = false }
-- |   "https://example.com"
-- | ```
-- |
-- | which is applied to [defaultOptions](#v:defaultOptions) to generate and updated `Record` of
-- | options.
parseWithOptions :: (ParserOptions -> ParserOptions) -> Url -> Aff ParsedContent
parseWithOptions ops url = do
  res <- toAffE $ FFI.parse url (encodeJson $ ops defaultOptions)
  case (decodeJson res) :: Either _ ParsedContent of
    Right x -> pure x
    Left  _ -> case (fixLabel <$> decodeJson res) :: Either _ ParsedContent of
      Right x -> pure x
      Left  _ -> case (decodeJson res) :: Either _ MercuryError of
        Right err -> throwError $ error err.message
        Left _    -> throwError $ error "Could not parse output from Mercury Parser"

  where
    fixLabel = rename (SProxy :: SProxy "pages_rendered") (SProxy :: SProxy "rendered_pages")

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
-- |
-- | For more information on what these options do see [ParserOptions](#t:ParserOptions).
defaultOptions :: ParserOptions
defaultOptions =
  { html: Nothing
  , fetchAllPages: true
  , fallback: true
  , contentType: Html
  , headers: Headers empty
  --, extend: Nothing
  , customExtractor: Nothing
  }
