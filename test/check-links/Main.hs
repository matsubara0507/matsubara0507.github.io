{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Prelude                   hiding (FilePath, null)

import           Data.List                 (nub, sort)
import           Data.Maybe                (fromMaybe)
import           Data.Text                 (Text, isPrefixOf, null, unpack)
import           Data.Traversable          (traverse)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status (statusIsSuccessful)
import           Shelly
import           Test.Hspec
import           Text.HTML.Scalpel.Core

main :: IO ()
main = do
  urls <- fmap mconcat . shelly $ do
    run_ "stack" ["exec", "--", "site", "build"]
    files <- ls "_site/posts"
    traverse (fmap scrapeLinks . readfile) files
  hspec . mapM_ spec . nub . sort $ filter check urls
  where
    check url = not . or . (:) (null url) $
      fmap (`isPrefixOf` url) ["https://matsubara0507.github.io", "../", "#"]
    spec url = it (unpack url) $ checkLink url `shouldReturn` True

scrapeLinks :: Text -> [Text]
scrapeLinks txt = fromMaybe [] $ scrapeStringLike txt scraper
  where
    scraper = attrs "href" "a"

checkLink :: Text -> IO Bool
checkLink url = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest $ unpack url
  statusIsSuccessful . responseStatus <$> httpNoBody request manager