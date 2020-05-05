module Test.Main where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..), singleton)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Error, Milliseconds(..), attempt, launchAff_, message)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual, shouldReturn)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpec')
import Text.Parsing.Mercury


-- Test --------------------------------------------------------------------------------------------

main :: Effect Unit
main = launchAff_ $ runSpec' (defaultConfig { timeout = Just $ Milliseconds 10000.0 }) [consoleReporter] do
  describe "jsonEncodeInstance" do
    it "FieldExtractor" $
      testEncodeJson
        { selectors: NonEmpty (Attribute (Tuple "foo" "bar")) [Plain "qux"] }
        """{"selectors":[["foo","bar"],"qux"]}"""
    it "ContentExtractor" $
      testEncodeJson
        { selectors: NonEmpty (MultiMatch (NonEmpty "foo" ["bar", "qux"])) [PlainC "quux"] }
        """{"selectors":[["foo","bar","qux"],"quux"]}"""

  describe "parse"  do
    it "parses a website" $
      parse dfArticle.url `shouldReturn` dfArticle
    it "does pagination" $
      ((_.rendered_pages) <$> parse arsArticle.url) `shouldReturn` arsArticle.rendered_pages
    it "returns an error if a malformed url is passed" $
      testError "foo.com" `shouldReturn` Left urlError
    it "returns an error on non-200 responses" $
      testError "https://www.google.com/foo" `shouldReturn` Left non200Error
    it "returns an error on invalid content types" $
      testError "https://www.google.com/favicon.ico" `shouldReturn` Left contentError

  describe "parsewithOptions" do
    describe "contenType" do
      it "returns text content if text is passed as contentType" $
        parseWithOptions _ { contentType = Text } dfArticleText.url `shouldReturn` dfArticleText
      it "returns markdown if markdown is passed as contentType" $
        parseWithOptions _ { contentType = Markdown } dfArticleMd.url `shouldReturn` dfArticleMd
    describe "fetchAllPages" $
      it "returns a single page of multi-page content when fetchAllPages is false" $
        shouldReturn
          ((_.rendered_pages) <$> parseWithOptions _ { fetchAllPages = false } arsArticle.url)
          1
    describe "customExtractor" $
      it "should be able to use a custom parser given as an option" $
        shouldReturn
          (parseWithOptions _ { customExtractor = Just dfCustomParser } dfArticleCustom.url)
          dfArticleCustom


-- Test helper functions ---------------------------------------------------------------------------

testError :: String -> Aff (Either String ParsedContent)
testError = map (lmap message) <<< attempt <<< parse

testEncodeJson :: forall m a. MonadThrow Error m => EncodeJson a => a -> String -> m Unit
testEncodeJson x y = stringify (encodeJson x) `shouldEqual` y

-- Test content ------------------------------------------------------------------------------------

dfArticle :: ParsedContent
dfArticle =
  { author: Nothing
  , content: Just "<div id=\"Main\"> <dl class=\"linkedlist tallenough\">\n<dt><a href=\"https://sixcolors.com/post/2020/04/magic-keyboard-ipad-pro-review/\">Jason Snell Reviews the 12.9-Inch iPad Magic&#xA0;Keyboard</a></dt>\n<dd>\n<p>Jason Snell, writing at Six Colors:</p> <blockquote> <p>This is basically my iPad dream, fulfilled. But dreams are amorphous things, and they fall apart if you begin to interrogate them logically. The Magic Keyboard for iPad Pro isn&#x2019;t a dream, it&#x2019;s a real product, one that&#x2019;s sitting in my lap right now. It&#x2019;s one thing for Apple to decide that it&#x2019;s time to offer a full laptop experience on the iPad&#x2009;&#x2014;&#x2009;and an altogether different thing to execute that vision.</p> <p>As I scrutinize the Magic Keyboard, it doesn&#x2019;t fall apart as if it were a dream&#x2009;&#x2014;&#x2009;it holds together, solidly. This is a product that isn&#x2019;t for everyone, to be sure&#x2026; but it&#x2019;s exactly what I&#x2019;ve been looking for.</p>\n</blockquote> <p>I really enjoyed Snell&#x2019;s review, and found it comforting that we came to extremely similar conclusions despite coming from quite different starting points. We&#x2019;re both longtime&#x2009;&#x2014;&#x2009;<em>very</em> longtime&#x2009;&#x2014;&#x2009; Mac aficionados, but he&#x2019;s been on <em>Team Do Work Using an iPad</em> for years now. Long story short, I have not. But for all the same reasons that he finds the iPad Magic Keyboard and newfound pointer support in iPadOS make working on iPad even better, I find they make it possible.</p> <p>I think that&#x2019;s the key to all this: Apple has made iPad better in new ways without making it worse in any existing way.</p> <p class=\"smallprint\">&#x2605; <em>Monday, 27 April 2020</em></p>\n</dd>\n</dl> </div>"
  , date_published: Just "2020-04-27T07:00:00.000Z"
  , dek: Nothing
  , direction: LTR
  , domain: "daringfireball.net"
  , excerpt: Just "Link to: https://sixcolors.com/post/2020/04/magic-keyboard-ipad-pro-review/"
  , lead_image_url: Just "https://daringfireball.net/graphics/df-square-1024.png"
  , next_page_url: Nothing
  , rendered_pages: 1
  , title: Just "Jason Snell Reviews the 12.9-Inch iPad Magic Keyboard"
  , total_pages: 1
  , url: "https://daringfireball.net/linked/2020/04/27/snell-ipad-magic-keyboard"
  , word_count: 238
  }

dfArticleText :: ParsedContent
dfArticleText = dfArticle { content = Just " \nJason Snell Reviews the 12.9-Inch iPad Magic Keyboard\n\nJason Snell, writing at Six Colors:  This is basically my iPad dream, fulfilled. But dreams are amorphous things, and they fall apart if you begin to interrogate them logically. The Magic Keyboard for iPad Pro isn’t a dream, it’s a real product, one that’s sitting in my lap right now. It’s one thing for Apple to decide that it’s time to offer a full laptop experience on the iPad — and an altogether different thing to execute that vision. As I scrutinize the Magic Keyboard, it doesn’t fall apart as if it were a dream — it holds together, solidly. This is a product that isn’t for everyone, to be sure… but it’s exactly what I’ve been looking for.\n I really enjoyed Snell’s review, and found it comforting that we came to extremely similar conclusions despite coming from quite different starting points. We’re both longtime — very longtime —  Mac aficionados, but he’s been on Team Do Work Using an iPad for years now. Long story short, I have not. But for all the same reasons that he finds the iPad Magic Keyboard and newfound pointer support in iPadOS make working on iPad even better, I find they make it possible. I think that’s the key to all this: Apple has made iPad better in new ways without making it worse in any existing way. ★ Monday, 27 April 2020\n\n " }

dfArticleMd :: ParsedContent
dfArticleMd = dfArticle { content = Just "[Jason Snell Reviews the 12.9-Inch iPad Magic Keyboard](https://sixcolors.com/post/2020/04/magic-keyboard-ipad-pro-review/)\n\nJason Snell, writing at Six Colors:\n\n> This is basically my iPad dream, fulfilled. But dreams are amorphous things, and they fall apart if you begin to interrogate them logically. The Magic Keyboard for iPad Pro isn’t a dream, it’s a real product, one that’s sitting in my lap right now. It’s one thing for Apple to decide that it’s time to offer a full laptop experience on the iPad — and an altogether different thing to execute that vision.\n> \n> As I scrutinize the Magic Keyboard, it doesn’t fall apart as if it were a dream — it holds together, solidly. This is a product that isn’t for everyone, to be sure… but it’s exactly what I’ve been looking for.\n\nI really enjoyed Snell’s review, and found it comforting that we came to extremely similar conclusions despite coming from quite different starting points. We’re both longtime — _very_ longtime —  Mac aficionados, but he’s been on _Team Do Work Using an iPad_ for years now. Long story short, I have not. But for all the same reasons that he finds the iPad Magic Keyboard and newfound pointer support in iPadOS make working on iPad even better, I find they make it possible.\n\nI think that’s the key to all this: Apple has made iPad better in new ways without making it worse in any existing way.\n\n★ _Monday, 27 April 2020_" }

arsArticle :: { rendered_pages :: Int , total_pages :: Int , url :: String }
arsArticle =
  { url: "https://arstechnica.com/gadgets/2016/08/the-connected-renter-how-to-make-your-apartment-smarter/"
  , total_pages: 3
  , rendered_pages: 3
  }

urlError :: String
urlError =  "The url parameter passed does not look like a valid URL. Please check your URL and try again."

contentError :: String
contentError = "Content does not appear to be text."

non200Error :: String
non200Error = "Resource returned a response status code of 404 and resource was instructed to reject non-200 status codes."

dfCustomParser :: CustomParser
dfCustomParser =
  { title: Just
    { selectors: singleton $ Attribute (Tuple "meta[charset]" "charset") }
  , author: Just
    { selectors: singleton $ Plain "div#Sidebar>p:first-child>strong" }
  , content: Just
    { selectors: singleton $ PlainC "dd", clean:  Just $ singleton "p.smallprint" }
  , date_published: Just
    { selectors: singleton $ Plain "dd>p.smallprint>em" }
  , excerpt: Just
    { selectors: singleton $ Plain "dd>p:first-child" }
  , lead_image_url: Just
    { selectors: singleton $ Attribute (Tuple "meta[property='og:image']" "content") }
  , domain: Just "daringfireball.net"
  , dek: Nothing
  , next_page_url:  Nothing
  }

dfArticleCustom :: ParsedContent
dfArticleCustom =
  { author: Just "John Gruber"
  , content: Just "<div><dd>\n<p>Jason Snell, writing at Six Colors:</p>\n\n<blockquote>\n  <p>This is basically my iPad dream, fulfilled. But dreams are amorphous things, and they fall apart if you begin to interrogate them logically. The Magic Keyboard for iPad Pro isn&#x2019;t a dream, it&#x2019;s a real product, one that&#x2019;s sitting in my lap right now. It&#x2019;s one thing for Apple to decide that it&#x2019;s time to offer a full laptop experience on the iPad&#x2009;&#x2014;&#x2009;and an altogether different thing to execute that vision.</p>\n\n<p>As I scrutinize the Magic Keyboard, it doesn&#x2019;t fall apart as if it were a dream&#x2009;&#x2014;&#x2009;it holds together, solidly. This is a product that isn&#x2019;t for everyone, to be sure&#x2026; but it&#x2019;s exactly what I&#x2019;ve been looking for.</p>\n</blockquote>\n\n<p>I really enjoyed Snell&#x2019;s review, and found it comforting that we came to extremely similar conclusions despite coming from quite different starting points. We&#x2019;re both longtime&#x2009;&#x2014;&#x2009;<em>very</em> longtime&#x2009;&#x2014;&#x2009; Mac aficionados, but he&#x2019;s been on <em>Team Do Work Using an iPad</em> for years now. Long story short, I have not. But for all the same reasons that he finds the iPad Magic Keyboard and newfound pointer support in iPadOS make working on iPad even better, I find they make it possible.</p>\n\n<p>I think that&#x2019;s the key to all this: Apple has made iPad better in new ways without making it worse in any existing way.</p>\n\n\n</dd></div>"
  , date_published: Just "2020-04-27T07:00:00.000Z"
  , dek: Nothing
  , direction: LTR
  , domain: "daringfireball.net"
  , excerpt: Just "Jason Snell, writing at Six Colors:"
  , lead_image_url: Just "https://daringfireball.net/graphics/df-square-1024.png"
  , next_page_url: Nothing
  , rendered_pages: 1
  , title: Just "UTF-8"
  , total_pages: 1
  , url: "https://daringfireball.net/linked/2020/04/27/snell-ipad-magic-keyboard"
  , word_count: 225
  }
