-- | Types for Mercury Parser wrapper.
module Text.Parsing.Mercury.Types
  ( ParsedContent
  , Url
  , TextDirection(..)
  , MercuryError
  , ParserOptions
  , ContentType(..)
  , Headers(..)
  , HeaderName
  , HeaderValue
  , CustomParser
  , FieldExtractor
  , ContentExtractor
  , FieldSelector(..)
  , ContentSelector(..)
  , Selector
  ) where

import Prelude

import Control.Alternative ((<|>))
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Generic.Rep (decodeLiteralSumWithTransform)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic.Rep (encodeLiteralSumWithTransform)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.NonEmpty (NonEmpty)
import Data.String (toLower)
import Data.Tuple (Tuple)
import Foreign.Object as FO


-- Mercury response types --------------------------------------------------------------------------

-- | Response type for Mercury Parser's `parse` function.
-- |
-- | **Wrapper implementation note**
-- |
-- | Mercury Parser's documentation doesn't make it clear which fields might be `null`. Fields with
-- | type `Maybe` are my current best guess, based on trolling some testing and skimming their code.
--
-- TODO:
-- * Morally all the `Int`s here should be `Natural`s, but due to not being able to define orphan
--   instances, decoding them to `Natural` is annoying.
-- * Same goes for `date_published` with `Date`.
-- * Troll through Mercury's code and figure out with more certainty which values can be null.
type ParsedContent =
  { title          :: Maybe String
  , content        :: Maybe String
  , author         :: Maybe String
  , date_published :: Maybe String
  , lead_image_url :: Maybe Url
  , dek            :: Maybe String
  , next_page_url  :: Maybe Url
  , url            :: Url
  , domain         :: String
  , excerpt        :: Maybe String
  , word_count     :: Int
  , direction      :: TextDirection
  , total_pages    :: Int
  , rendered_pages :: Int
  }

type Url = String

-- | Direction parsed content is written in.
data TextDirection
  -- | Left to right
  = LTR
  -- | Right to left
  | RTL

derive instance eqTextDirection      :: Eq TextDirection
derive instance ordTextDirection     :: Ord TextDirection
derive instance genericTextDirection :: Generic TextDirection _
instance showTextDirection :: Show TextDirection where
  show = genericShow
-- | The constructors `LTR` and `RTL` are decoded from the strings `"ltr"` and `"rtl"` respectively.
instance decodeJsonTextDirection :: DecodeJson TextDirection where
  decodeJson = decodeLiteralSumWithTransform toLower
-- | The constructors `LTR` and `RTL` are encoded to the strings `"ltr"` and `"rtl"` respectively.
instance encodeJsonTextDirection :: EncodeJson TextDirection where
  encodeJson = encodeLiteralSumWithTransform toLower


-- | Error type for Mercury Parser responses.
-- |
-- | **Wrapper implementation note**
-- |
-- | Mercury Parser reports errors in two ways. Sometimes it throws, and other times the `Promise`
-- | resolves with a object with this shape. In order to make the return type of the `parse` and
-- |`parseWithOptions` wrapper functions consistent, we check whether the `Promise` resolved
-- | with an error shaped object, and if so throw.
type MercuryError =
  { error   :: Boolean
  , message :: String
  }


-- Mercury request types ---------------------------------------------------------------------------

-- | Options accepted by Mercury Parser's `parse` function:
-- |
-- | * `html`: HTML for the parser to parse instead of fetching the content of the URL.
-- | * `fatchAllPages`: whether to fetch all pages of multi-page content.
-- | * `fallback`: whether to the fallback parser if the any custom parsers (either built in to
-- |   Mercury Parser, or provided via `customExtractor`) fail.
-- | * `contentType`: the content type that Mercury Parser will parse the website content into. See
-- |   [ContentType](#t:ContentType) for options.
-- | * `headers`: custom headers for Mercury Parser to use when fetching the content, see
-- |    [Headers](#t:Headers).
-- | * `customExtractor`: the specification for a custom extractor to use when parsing, see
-- |    [CustomExtractor](#t:CustomExtractor).
-- |
-- | **Wrapper implementation note**
-- |
-- | Mercury Parser also has and `extend` option to define additional extractors for custom fields.
-- | This option hasn't been implemented yet.
--
-- TODO: Implement `extend`
type ParserOptions =
  { html             :: Maybe String
  , fetchAllPages    :: Boolean
  , fallback         :: Boolean
  , contentType      :: ContentType
  , headers          :: Headers
  --, extend :: Maybe { | re}
  , customExtractor  :: Maybe CustomParser
  }

-- | The type of content Mercury Parser returns in the `content` field.
data ContentType = Html | Markdown | Text

derive instance eqContentType      :: Eq ContentType
derive instance ordContentType     :: Ord ContentType
derive instance genericContentType :: Generic ContentType _
instance showContentType :: Show ContentType where
  show = genericShow
-- | The constructors `Html`, `Markdown`, and `Text` are decoded from the strings `"html"`,
-- | `"markdown"`, and `"text"` respectively.
instance decodeJsonContentType :: DecodeJson ContentType where
  decodeJson = decodeLiteralSumWithTransform toLower
-- | The constructors `Html`, `Markdown`, and `Text` are encoded to the strings `"html"`,
-- | `"markdown"`, and `"text"` respectively.
instance encodeJsonContentType :: EncodeJson ContentType where
  encodeJson = encodeLiteralSumWithTransform toLower

-- | Custom headers for Mercury Parser to use when fetching content to parse.
newtype Headers = Headers (Map HeaderName HeaderValue)

derive instance eqHeader      :: Eq Headers
derive instance ordHeader     :: Ord Headers
derive instance genericHeader :: Generic Headers _
derive instance newtypeHeader :: Newtype Headers _
instance showHeader :: Show Headers where
  show = genericShow
-- | Decodes a JS object of key value pairs representing header names and values into `Headers`.
-- TODO: See if we can define these more concisely using generics
instance decodeJsonHeader :: DecodeJson Headers where
  decodeJson = map (wrap <<< Map.fromFoldable <<< (FO.toUnfoldable :: _ -> Array _)) <$> decodeJson
-- | Encodes `Headers` into a JS object of key value pairs representing header names and values.
-- TODO: See if we can define these more concisely using generics
instance encodeJsonHeader :: EncodeJson Headers where
  encodeJson = encodeJson <<< FO.fromFoldable <<< (Map.toUnfoldable :: _ -> Array _) <<< unwrap

type HeaderName  = String
type HeaderValue = String


-- Custom parser types -----------------------------------------------------------------------------

-- | The specification of a custom parser for Mercury Parser.
-- |
-- | See Mercury Parser's documentation on [Custom Parsers](https://github.com/postlight/mercury-parser/blob/master/src/extractors/custom/README.md)
-- | for more details.
-- |
-- | **Wrapper implementation note**
-- |
-- | Although all the types are defined, using a custom parser has not been tested yet. Use at
-- | your own risk.
type CustomParser =
  { title          :: Maybe FieldExtractor
  , author         :: Maybe FieldExtractor
  , content        :: Maybe ContentExtractor
  , date_published :: Maybe FieldExtractor
  , lead_image_url :: Maybe FieldExtractor
  , dek            :: Maybe FieldExtractor
  , next_page_url  :: Maybe FieldExtractor
  , excerpt        :: Maybe FieldExtractor
  , domain         :: Maybe String
  }


-- | Extractor for non-`content` fields.
-- |
-- | A list of CSS selectors to use to find the element on the page that contains the data for a
-- | given field in the parsers output.
-- |
-- | See [FieldSelector](#t:FieldSelector).
type FieldExtractor = { selectors :: NonEmpty Array FieldSelector }

-- | Extractor for the `content` field.
-- |
-- | See [ContentSelector](#t:ContentSelector) for information on how content selectors work.
-- |
-- | The `clean` field is used to provide a list of CSS selectors for elements that should be
-- | removed from the content extracted by the `selectors`.
-- |
-- | **Wrapper implementation note**
-- |
-- | Mercury Parser also allows the inclusion of a [transforms](https://github.com/postlight/mercury-parser/blob/master/src/extractors/custom/README.md#using-transforms)
-- | field for content extractors. This field hasn't been implemented yet.
--
-- TODO: Implement `transforms`.
type ContentExtractor =
  { selectors :: NonEmpty Array ContentSelector
  , clean :: Maybe (NonEmpty Array Selector)
  --, transforms ::
  }

-- | A CSS selector for extracting content from a webpage for non-`content` fields.
-- |
-- | See Mercury Parser's documentation for [Basic selectors](https://github.com/postlight/mercury-parser/blob/master/src/extractors/custom/README.md#basic-selectors),
-- | and [Selecting an attribute](https://github.com/postlight/mercury-parser/blob/master/src/extractors/custom/README.md#selecting-an-attribute)
-- | for information about writing these selectors.
data FieldSelector = Plain Selector | Attribute (Tuple Selector Selector)
derive instance eqFieldSelector      :: Eq FieldSelector
derive instance ordFieldSelector     :: Ord FieldSelector
derive instance genericFieldSelector :: Generic FieldSelector _
instance showFieldSelector :: Show FieldSelector where
  show = genericShow
-- | Decodes a string to a `Plain` selector, and a two element array to an `Attribute` selector.
-- TODO: See if we can define these more concisely using generics
instance decodeJsonFieldSelector :: DecodeJson FieldSelector where
  decodeJson x
    =   (Plain <$> ((decodeJson x) :: Either _ Selector))
    <|> (Attribute <$> ((decodeJson x) :: Either _ (Tuple Selector Selector)))
-- | Encodes a `Plain` selector into a string, and `Attribute` selector into a two element array.
-- TODO: See if we can define these more concisely using generics
instance encodeJsonFieldSelector :: EncodeJson FieldSelector where
  encodeJson (Plain s) = encodeJson s
  encodeJson (Attribute xs) = encodeJson xs

-- | A CSS selector for extracting content from a webpage for the `content` field.
-- |
-- | See Mercury Parser's documentation for [Basic selectors](https://github.com/postlight/mercury-parser/blob/master/src/extractors/custom/README.md#basic-selectors),
-- | and [Content selectors](https://github.com/postlight/mercury-parser/blob/master/src/extractors/custom/README.md#content-selectors)
-- | for information about writing these selectors.
data ContentSelector = PlainC Selector | MultiMatch (NonEmpty Array Selector)
derive instance eqContentSelector      :: Eq ContentSelector
derive instance ordContentSelector     :: Ord ContentSelector
derive instance genericContentSelector :: Generic ContentSelector _
instance showContentSelector :: Show ContentSelector where
  show = genericShow
-- | Decodes a string to a `PlainC` selector, and a non-empty array to an `Attribute` selector.
-- TODO: See if we can define these more concisely using generics
instance decodeJsonContentSelector :: DecodeJson ContentSelector where
  decodeJson x
    =   (PlainC <$> ((decodeJson x) :: Either _ Selector))
    <|> (MultiMatch <$> ((decodeJson x) :: Either _ (NonEmpty Array Selector)))
-- | Encodes a `PlainC` selector into a string, and `MultiMatch` selector into a array.
-- TODO: See if we can define these more concisely using generics
instance encodeJsonContentSelector :: EncodeJson ContentSelector where
  encodeJson (PlainC s) = encodeJson s
  encodeJson (MultiMatch xs) = encodeJson $ xs

type Selector = String
