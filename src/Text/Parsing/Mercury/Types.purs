module Text.Parsing.Mercury.Types where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (decodeLiteralSumWithTransform)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Generic.Rep (encodeLiteralSumWithTransform)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)
import Data.Map (Map, toUnfoldable)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.NonEmpty (NonEmpty)
import Data.String (toLower)
import Data.Tuple (Tuple(..))
import Foreign.Object (fromFoldable)

-- | Type alias for strings that are urls
type Url = String


-- Text Direction ----------------------------------------------------------------------------------

-- | Type representing whether text is read from left to right (`LTR`) or right to left (`RTL`).
data TextDirection = LTR | RTL

-- Automatic instance deriving
derive instance eqTextDirection      :: Eq TextDirection
derive instance ordTextDirection     :: Ord TextDirection
derive instance genericTextDirection :: Generic TextDirection _

-- Manual instance deriving
instance showTextDirection :: Show TextDirection where
  show = genericShow
instance decodeJsonTextDirection :: DecodeJson TextDirection where
  decodeJson = decodeLiteralSumWithTransform toLower
instance encodeJsonTextDirection :: EncodeJson TextDirection where
  encodeJson = encodeLiteralSumWithTransform toLower


-- Mercury response types --------------------------------------------------------------------------

-- | Type returned Mercury Parser
-- TODO:
-- * Morally all the `Int`s here should be `Natural`s, but due to not being able to define orphan
--   instances decoding them to `Natural` would be a huge pain in the ass.
-- * Same goes for `date_published` with `Date`.
-- * Troll through Mercury code and figure out with more certainty which values can be null.
type ParseResponse =
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

-- | Error type for Mercury Parser
type ErrorResponse =
  { error   :: Boolean
  , message :: String
  }


-- Mercury request types ---------------------------------------------------------------------------

-- | Content type returned by by Mercury Parser
data ResponseContentType = Html | Markdown | Text

-- Automatic instance deriving
derive instance eqResponseContentType      :: Eq ResponseContentType
derive instance ordResponseContentType     :: Ord ResponseContentType
derive instance genericResponseContentType :: Generic ResponseContentType _

-- Manual instance deriving
instance showResponseContentType :: Show ResponseContentType where
  show = genericShow
instance decodeJsonResponseContentType :: DecodeJson ResponseContentType where
  decodeJson = decodeLiteralSumWithTransform toLower
instance encodeJsonResponseContentType :: EncodeJson ResponseContentType where
  encodeJson = encodeLiteralSumWithTransform toLower


type Selector = String

data NonContentSelector = Plain Selector | Attribute (Tuple Selector Selector)

-- Automatic instance deriving
derive instance eqNonContentSelector      :: Eq NonContentSelector
derive instance ordNonContentSelector     :: Ord NonContentSelector
derive instance genericNonContentSelector :: Generic NonContentSelector _

-- Manual instance deriving
instance showNonContentSelector :: Show NonContentSelector where
  show = genericShow
instance encodeJsonNonContentSelector :: EncodeJson NonContentSelector where
  encodeJson (Plain s) = encodeJson s
  encodeJson (Attribute (Tuple s1 s2)) = encodeJson [s1, s2]


data ContentSelector = PlainC Selector | MultiMatch (NonEmpty List Selector)

-- Automatic instance deriving
derive instance eqContentSelector      :: Eq ContentSelector
derive instance ordContentSelector     :: Ord ContentSelector
derive instance genericContentSelector :: Generic ContentSelector _

-- Manual instance deriving
instance showContentSelector :: Show ContentSelector where
  show = genericShow
instance encodeJsonContentSelector :: EncodeJson ContentSelector where
  encodeJson (PlainC s) = encodeJson s
  encodeJson (MultiMatch xs) = encodeJson $ xs


type NonContentExtractor = { selectors :: NonEmpty List NonContentSelector }
type ContentExtractor =
  { selectors :: NonEmpty List ContentSelector
  , clean :: Maybe (NonEmpty List Selector)
  --, transforms ::
  }

type CustomExtractor =
  { title          :: Maybe NonContentExtractor
  , author         :: Maybe NonContentExtractor
  , content        :: Maybe ContentExtractor
  , date_published :: Maybe NonContentExtractor
  , lead_image_url :: Maybe NonContentExtractor
  , dek            :: Maybe NonContentExtractor
  , next_page_url  :: Maybe NonContentExtractor
  , excerpt        :: Maybe NonContentExtractor
  , domain         :: Maybe String
  }

newtype Headers = Headers (Map String String)

-- Automatic instance deriving
derive instance eqHeader      :: Eq Headers
derive instance ordHeader     :: Ord Headers
derive instance genericHeader :: Generic Headers _
derive instance newtypeHeader :: Newtype Headers _

-- Manual instance deriving
instance showHeader :: Show Headers where
  show = genericShow
instance encodeJsonHeader :: EncodeJson Headers where
  encodeJson (Headers hs) = encodeJson <<< fromFoldable <<< (toUnfoldable :: Map _ _ -> Array _) $ hs


type ParseOptions =
  { html :: Maybe String
  , fetchAllPages :: Boolean
  , fallback :: Boolean
  , contentType :: ResponseContentType
  , headers :: Headers
  --, extend :: Maybe { | re}
  , customExtractor :: Maybe CustomExtractor
  }
