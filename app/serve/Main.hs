{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Maybe                    (fromMaybe, listToMaybe)
import           Network.Wai.Middleware.Static
import           System.Environment            (getArgs)
import           Web.Scotty

main :: IO ()
main = do
  path <- fromMaybe "docs" . listToMaybe <$> getArgs
  scotty 8080 $ do
    middleware $
      staticPolicy $ addBase path >-> noDots

    get "/" $
      redirect "/index.html"
