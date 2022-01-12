{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils where

import           Control.Monad          (forM)
import           Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Lazy   as LBS
import           Data.List              (isPrefixOf)
import qualified Data.Text              as Text
import           Data.Void              (Void)
import           Network.HTTP.Req       (req, runReq)
import qualified Network.HTTP.Req       as Req
import           Text.HTML.Scalpel.Core (Scraper, attr, scrapeStringLike, (@:),
                                         (@=))
import           Text.Megaparsec        (Parsec, manyTill, parseMaybe)
import qualified Text.Megaparsec.Char   as Parser
import           Text.StringLike        (StringLike (toString))
import           Text.URI               (mkURI)

replaceLinkToOGImage :: MonadIO m => String -> m String
replaceLinkToOGImage  md =
  fmap unlines $ forM (lines md) $ \line ->
    if "[og:image" `isPrefixOf` line then
      case parseMaybe ogImageTagParser line of
        Just (attrs, url) ->
          maybe line (buildEmbedImage attrs url) <$> fetchOGImage url
        Nothing ->
          pure line
    else
      pure line

type Parser = Parsec Void String

ogImageTagParser :: Parser (String, String)
ogImageTagParser = do
  _ <- Parser.string "[og:image"
  attrs <- Parser.printChar `manyTill` Parser.char ']'
  _ <- Parser.char '('
  url <- Parser.printChar `manyTill` Parser.char ')'
  pure (attrs, url)

fetchOGImage :: MonadIO m => String -> m (Maybe String)
fetchOGImage url =
  case Req.useHttpsURI =<< mkURI (Text.pack url) of
    Nothing ->
      pure Nothing

    Just (url', opts) -> do
      html <- fetchHtml url' opts
      pure $ scrapeStringLike html ogimgaeScraper

fetchHtml :: MonadIO m => Req.Url 'Req.Https -> Req.Option 'Req.Https -> m LBS.ByteString
fetchHtml url opts =
  runReq Req.defaultHttpConfig $
    Req.responseBody <$> req Req.GET url Req.NoReqBody Req.lbsResponse opts

ogimgaeScraper :: (Show s, StringLike s) => Scraper s String
ogimgaeScraper = toString <$> attr "content" ("meta" @: ["property" @= "og:image"])

buildEmbedImage :: String -> String -> String -> String
buildEmbedImage attrs url image =
  "[<img src=\"" ++ image ++ "\"" ++ attrs ++ " />](" ++ url ++ ")"
