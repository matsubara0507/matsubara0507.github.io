module Spec.Utils (spec) where

import           Test.Tasty.Hspec
import           Text.HTML.Scalpel.Core (scrapeStringLike)
import           Text.Megaparsec        (parseMaybe)
import           Utils

spec :: Spec
spec = do
  describe "ogImageTagParser" $ do
    it "should be Nothing" $
      parseMaybe ogImageTagParser "" `shouldBe` Nothing

    it "should be Nothing" $
      parseMaybe ogImageTagParser "[](https://example.com)" `shouldBe` Nothing

    it "should be parsed url" $
      parseMaybe ogImageTagParser "[og:image](https://example.com)" `shouldBe` Just ("", "https://example.com")

    it "should be parsed url" $
      parseMaybe ogImageTagParser "[og:image ](https://example.com)" `shouldBe` Just (" ", "https://example.com")

    it "should be parsed attributes and url" $
      parseMaybe ogImageTagParser "[og:image style=\"width:500px\"](https://example.com)" `shouldBe` Just (" style=\"width:500px\"", "https://example.com")

    it "should be parsed attributes and url" $
      parseMaybe ogImageTagParser "[og:image style=\"width:500px\" id=\"hoge\"](https://example.com)" `shouldBe` Just (" style=\"width:500px\" id=\"hoge\"", "https://example.com")

    it "should be parsed empty url" $
      parseMaybe ogImageTagParser "[og:image]()" `shouldBe` Just ("", "")


  describe "ogimgaeScraper" $ do
    it "should be Nothing" $
      scrapeStringLike "" ogimgaeScraper `shouldBe` Nothing

    it "should be Nothing" $
      scrapeStringLike "<head></head>" ogimgaeScraper `shouldBe` Nothing


  describe "buildEmbedImage" $ do
    it "should be empty img tag" $
      buildEmbedImage "" "" "" `shouldBe` "[<img src=\"\" />]()"

    it "should be img tag" $
      buildEmbedImage "" "https://example.com" "image-url" `shouldBe` "[<img src=\"image-url\" />](https://example.com)"

    it "should be img tag" $
      buildEmbedImage " style=\"width:500px\" id=\"hoge\"" "https://example.com" "image-url" `shouldBe` "[<img src=\"image-url\" style=\"width:500px\" id=\"hoge\" />](https://example.com)"
