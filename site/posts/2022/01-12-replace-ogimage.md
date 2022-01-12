---
title: GitHub の URL を自動で OGP 画像に置き換える
tags: [Haskell]
---

いつぞやから GitHub の各種 URL はいい感じの OGP 画像を埋め込んでくれるようになりました。

- [OpenGraph images for GitHub repositories, commits, issues, and pull requests | GitHub Changelog](https://github.blog/changelog/2021-04-21-opengraph-images-for-github-repositories-commits-issues-and-pull-requests/)

前に、こういう GitHub カード的なのが欲しくて Elm でいい感じにカードを構築するツールを作ってましたが、本家の方がかっこいいので置き換えることにします。
本記事はそのメモ書きです。

## こんな感じ

URLを `[og:image](url)` って感じに書いておくと、こんな感じになります。

[og:image style="max-width: 500px;"](https://github.com/matsubara0507/matsubara0507.github.io)

`og:image` の後に `style="width: 500px"` などを記述すると、そのまま HTML の img タグの属性として利用してくれる仕様です。

## 実装

このサイトは [Slick](https://hackage.haskell.org/package/slick) というツールを使っています。
Markdown から HTML への変換は、内部的にはファイルを読み込んで [Pandoc](https://hackage.haskell.org/package/pandoc) にかけるだけです：

```hs
buildPost :: FilePath -> Action Post
buildPost srcPath = cacheAction ("build" :: T.Text, srcPath) $ do
  postContent <- readFile' srcPath
  postData    <- markdownToHTML' @(Record FrontMatterParams) (T.pack postContent)
  ...
```

`markdownToHTML'` で Pandoc を使っています。
`postContent` はただの `String` 型の値で、Markdown のテキストです。
なので、これに簡単な置換処理をかけます：

```hs
buildPost :: FilePath -> Action Post
buildPost srcPath = cacheAction ("build" :: T.Text, srcPath) $ do
  postContent <- replaceLinkToOGImage =<< readFile' srcPath
  postData    <- markdownToHTML' @(Record FrontMatterParams) (T.pack postContent)
  ...

replaceLinkToOGImage :: MonadIO m => String -> m String
replaceLinkToOGImage md =
  fmap unlines $ forM (lines md) $ \line ->
    case parseMaybe ogImageTagParser line of
      Just (attrs, url) ->
        maybe line (buildEmbedImage attrs url) <$> fetchOGImage url
      Nothing ->
        pure line
```

各行に対してまず、`[og:image attrs](url)` をパースして属性とURLを取り出します。
その URL 先の HTML を取得し、`og:image` のメタタグから画像の URL を取り出します。
そして、それらを元にして Markdown をいい感じに置き換えます。

### パース

パースには [megaparsec](https://hackage.haskell.org/package/megaparsec) パッケージを使います：

```hs
type Parser = Parsec Void String

ogImageTagParser :: Parser (String, String)
ogImageTagParser = do
  _ <- Parser.string "[og:image"
  attrs <- Parser.printChar `manyTill` Parser.char ']'
  _ <- Parser.char '('
  url <- Parser.printChar `manyTill` Parser.char ')'
  pure (attrs, url)
```

`manyTill` を使うことで2つ目のパーサーが成功するまで1つ目のパーサーを繰り返し実行します。
結果として `]` が出るまでの文字列をパースするなどができ、この方法で属性とURLを取得しました。

### スクレイピング

HTMLの取得には [req](https://hackage.haskell.org/package/req) パッケージを使いました（割愛）。
そして、取得した HTML から任意の HTML 要素を取得するには [scalpel-core](https://hackage.haskell.org/package/scalpel-core) パッケージを使います：

```hs
fetchOGImage :: MonadIO m => String -> m (Maybe String)
fetchOGImage url =
  case Req.useHttpsURI =<< mkURI (Text.pack url) of
    Nothing ->
      pure Nothing
    Just (url', opts) -> do
      html <- fetchHtml url' opts
      pure $ scrapeStringLike html ogimgaeScraper

ogimgaeScraper :: (Show s, StringLike s) => Scraper s String
ogimgaeScraper = toString <$> attr "content" ("meta" @: ["property" @= "og:image"])
```

`property="og:image"` を持つ `meta` HTML タグの `content` 属性を取得しているだけです。
`scrapeStringLike` によって `Maybe String` として、その結果を取得しています。

### 置き換える

最後は集めた要素を利用して、いい感じに置き換えるだけです。
画像の拡大縮小をしたいので `![](url)` ではなく `<img src="url">` を使いました：

```hs
buildEmbedImage :: String -> String -> String -> String
buildEmbedImage attrs url image =
  "[<img src=\"" ++ image ++ "\"" ++ attrs ++ " >](" ++ url ++ ")"
```

これを Pandoc に食わせるだけで、いい感じな HTML にしてくれます。

## おまけ：静的ファイルのサーブ

Hakyll と異なり、Slick は静的ファイルのビルドまでで、localhost でサーブするような機能は提供していません。
[slick-template](https://github.com/ChrisPenner/slick-template) には、[npm の serve パッケージ](https://www.npmjs.com/package/serve) を利用する例が書いてありますが、できれば Haskell だけでなんとかしたいですよね。

ということで、[scotty](https://hackage.haskell.org/package/scotty) を利用して簡単な静的ファイルをサーブするやつを作りました：

```hs
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Maybe                    (fromMaybe, listToMaybe)
import           Network.Wai.Middleware.Static (staticPolicy, addBase, (>->), noDots)
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
```

scotty は Ruby の Sinatra にインスパイアされた極めてシンプルな Web フレームワークです。
静的ファイルをサーブするのには [wai-middleware-static](https://hackage.haskell.org/package/wai-middleware-static) パッケージを使っています。

## おしまい
