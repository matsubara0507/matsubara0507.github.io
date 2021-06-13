---
title: ここを Hakyll から Slick に移行してみた
tags: [site, Haskell]
---

特に深い理由はないですが，新しいツールを触ってみようかと思い変えてみました。

## Slick

[Slick](https://github.com/ChrisPenner/slick) は Hakyll と同じような Haskell 製の静的サイトジェネレーターで，サイトの生成方法自体を自身でプログラミングする．
[GitHub の README 曰く](https://github.com/ChrisPenner/slick#another-static-site-generator-what-about-hakylljekyll)，Hakyll はモナドに隠蔽されすぎてよくわからないから，もっとわかりやすいのを作った（超意訳）だそうだ．
実際，両方同じようなコードを書いてみた感じ，確かに Slick の方がわかりやすい（シンプル）．

Slick は内部的な処理の多くを外部パッケージに委ねている：

- ビルドシステムには [Shake](https://shakebuild.com/) を利用している（提供するサブコマンドやビルド結果のキャッシュなど）
- Markdown から HTML への変換は [Pandoc](https://hackage.haskell.org/package/pandoc) を利用している（Hakyll と同じ）
- テンプレートのレンダリングには [Mustache](https://hackage.haskell.org/package/mustache) を利用している

それぞれについては，あまり詳しいことを僕は知らないので，ここでは解説しません．

## カスタマイズする

Slick の作者は [ChrisPenner/slick-template](https://github.com/ChrisPenner/slick-template) というテンプレートリポジトリを用意しているので，これをベースにカスタマイズしていく．
正直なところ，半分は元の Hakyll でのテンプレートを再現するため．

### extensible レコード

まずはいきなりテンプレートの再現ではないやつ．

slick-template で使っていたレコード型を extensible レコードに置き換える．
例えば：

```haskell
type SiteMeta = Record
  '[ "siteTitle"   >: String
   , "domain"      >: String
   , "author"      >: String
   , "description" >: String
   , "twitter"     >: Maybe String
   , "github"      >: Maybe String
   ]

type Post = Record ('[ "date" >: String, "url" >: String ] ++ FrontMatterParams)

type FrontMatterParams =
  '[ "title"   >: String
   , "tags"    >: [Tag]
   , "image"   >: Maybe String
   , "content" >: String
   ]
```

フロントマターの部分だけ分けてるのは後述．

extensible を使うのに利点はあって，slick-template では `substitute` に渡す `ToMustache k` の値を結合するときに aeson の `Value` 型に変換して無理やり足しているが，extensible レコードであれば `happend` だけですむ．
無論このためには extensible レコードを `ToMustache` 型クラスのインスタンスにする必要がある：

```haskell
deriving instance ToMustache (h (TargetOf kv)) => ToMustache (Field h kv)

deriving instance ToMustache a => ToMustache (Identity a)

instance Forall (KeyTargetAre KnownSymbol (Instance1 ToMustache h)) xs => ToMustache (xs :& Field h) where
  toMustache = Object . hfoldlWithIndexFor
    (Proxy @ (KeyTargetAre KnownSymbol (Instance1 ToMustache h)))
    (\k m v -> HM.insert (stringKeyOf k) (toMustache v) m)
    HM.empty
```

また，Shake のキャッシュ（`cacheAction`）を利用するには生成物の型（例えば `Post`）が `Binary` 型クラスのインスタンスになってないといけない：

```haskell
deriving instance Binary (h (TargetOf kv)) => Binary (Field h kv)

instance Forall (KeyTargetAre KnownSymbol (Instance1 Binary h)) xs => Binary (xs :& Field h) where
    get = hgenerateFor
      (Proxy @ (KeyTargetAre KnownSymbol (Instance1 Binary h)))
      (const Binary.get)

    put = flip appEndo (return ()) . hfoldMap getConst .
      hzipWith
        (\(Comp Dict) x -> Const $ Endo $ (Binary.put x >>))
        (library :: xs :& Comp Dict (KeyTargetAre KnownSymbol (Instance1 Binary h)))
```

これは[過去に extensible 本体にあったインスタンス](https://github.com/fumieval/extensible/commit/d6e067e805f92a7c336fd4cc68042985ee13e6b8)を参考にした（今は実装されてない，理由は知らない）．
一応 [`decode . encode == id` という性質](https://hackage.haskell.org/package/binary-0.8.8.0/docs/Data-Binary.html#t:Binary)は満たしているっぽいので大丈夫だろう．

#

さて，例えば以上を踏まえて `buildPost` を書き換えると次のようになった（[元はこんな感じ](https://github.com/ChrisPenner/slick-template/blob/129b85152a481db19efc5e65e80b55a52af4a985/app/Main.hs#L100-L112)）：

```haskell
buildPost :: FilePath -> Action Post
buildPost srcPath = cacheAction ("build" :: T.Text, srcPath) $ do
  postContent <- readFile' srcPath
  postData    <- markdownToHTML' @(Record FrontMatterParams) (T.pack postContent)
  let postUrl   = dropDirectory1 (srcPath -<.> "html")
      postData' = happend siteMeta $ #url @= postUrl <: #date @= "..." <: postData
  template <- compileTemplate' "site/templates/post.html"
  writeFile' (outputFolder </> postUrl) $ T.unpack (substitute template postData')
  convert postData'
```

日付（`date` フィールド）については後述．
`markdownToHTML' :: FromJSON a => Text -> Action a` は本文を Markdown から HTML に変換して型 `a` の `content` フィールドへ格納し，残りのフィールドをフロントマターとしてパースする．
`TypeApplication` 言語拡張でフロントマターの型を明記してるのは，具体的な型がはっきりしていないと `happend` できないからだ．
ちなみに，今回定義した `FrontMatterParams` 型はタイトルとタグとサムネイル用画像をフロントマターとして与えている．

### 記事のパスから投稿日を出す

slick-template では投稿日をフロントマターで指定していたが，このサイトでは記事のパス（`YYYY/MM-DD-name.md`）で指定していた．
なので，そのような動作をするように修正する：

```haskell
buildPost :: FilePath -> Action Post
buildPost srcPath = cacheAction ("build" :: T.Text, srcPath) $ do
  postContent <- readFile' srcPath
  postData    <- markdownToHTML' @(Record FrontMatterParams) (T.pack postContent)
      -- YYYY/MM-DD-name.md から YYYY-MM-DD-name.html にしている 
  let postUrl   = dropDirectory1 (takeDirectory srcPath <> "-" <> takeFileName srcPath -<.> "html")
      postData' = happend siteMeta $ #url @= postUrl <: #date @= formatToHumanDate srcPath <: postData
  ... -- 割愛

formatToHumanDate :: FilePath -> String
formatToHumanDate p = formatTime defaultTimeLocale "%b %e, %Y" parsedTime
  where
    parsedTime = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" (year <> "-" <> date) :: UTCTime
    date = take 5 $ takeFileName p
    year = takeFileName $ takeDirectory p
```

ちなみに，このパス操作系の関数は `Development.Shake.FilePath` にあるのを利用している．

### ページネーション

slick-template では，記事の一覧がインデックスページにズラーっといくらでも並ぶようになっている．
これを10記事ぐらいずつに分けて表示できるようにする：

```haskell
buildArchive :: [Post] -> Action ()
buildArchive posts = do
  archiveT <- compileTemplate' "site/templates/archive.html"
  -- posts が古い順なので reverse している
  buildWithPagenation archiveT siteMeta (reverse posts) (outputFolder </> "archive")

buildWithPagenation
  :: Forall (KeyTargetAre KnownSymbol (Instance1 ToMustache Identity)) (xs ++ PagenationInfoParams)
  => Template
  -> (xs :& Field Identity)
  -> [Post]
  -> FilePath
  -> Action ()
buildWithPagenation t r posts dir = go 1 posts
  where
    pageSize = 10

    go :: Int -> [Post] -> Action ()
    go _ [] = pure ()
    go n posts' = do
      let info = #posts @= take pageSize posts'
              <: #prevPageNum @= guarded (> 0) (n - 1)
              <: #nextPageNum @= guarded (const $ length posts' > pageSize) (n + 1)
              <: nil
      writeFile' (dir </> show n -<.> "html") $ T.unpack (substitute t $ happend r info)
      go (n + 1) (drop pageSize posts')

    guarded :: (a -> Bool) -> a -> Maybe a
    guarded p a = if p a then Just a else Nothing
```

`buildWithPagenation` がページネーションしてくれる本体で，あとでタグページでも利用したいので別関数に切り出している．
単純に `posts` を分割するだけではダメで，現在と前後のページ番号をテンプレートに渡してあげる必要がある．
そのために簡単な再帰処理をしている．

ちなみに，`buildWithPagenation` の型が仰々しいのは `happend` するメタデータを任意の extensible レコードにしたかったからだ．
型パズルに悩んだ結果，型を書かないときに [HLS](https://github.com/haskell/haskell-language-server) がサジェストしてくれた型をそのまま書いたら通った（パズルできてないじゃん）．
HLS 最高．

### タグページ

slick-template では，タグをフロントマターに記述できるようになってはいるものの，タグページはないので自作した：

```haskell
buildTagPages :: [Post] -> Action [(Tag, Int)]
buildTagPages posts = do
  tagT <- compileTemplate' "site/templates/tags.html"
  forM (groupByTag posts) $ \(tag, posts') -> do
    buildWithPagenation tagT (#tag @= tag <: siteMeta) posts' (outputFolder </> "tags" </> tag)
    pure (tag, length posts')

groupByTag :: [Post] -> [(Tag, [Post])]
groupByTag = HML.toList . foldl go mempty
  where
    go :: HML.HashMap Tag [Post] -> Post -> HML.HashMap Tag [Post]
    go acc post =
      foldl (\acc' tag -> HML.insertWith (++) tag [post] acc') acc (post ^. #tags)
```

前述したとおり，こっちでも `buildWithPagenation` を使っているが，`siteMeta` の他にタグの情報もテンプレートに渡したかったので仰々しい型にしたのだ．
`buildTagPages` がタグ情報を返しているのはインデックスページに [Hakyll のタグクラウド](https://hackage.haskell.org/package/hakyll-4.14.0.0/docs/Hakyll-Web-Tags.html#v:renderTagCloud)を設定したいからだ：

```haskell
buildIndex :: [(Tag, Int)] -> [Post] -> Action ()
buildIndex tags posts = do
  indexT <- compileTemplate' "site/templates/index.html"
  let indexHTML = T.unpack $ substitute indexT (happend siteMeta indexInfo)
  writeFile' (outputFolder </> "index.html") indexHTML
  where
    indexInfo = #tags @= tagsInfo <: #posts @= take 4 (reverse posts) <: nil :: IndexInfo

    tagsInfo = map (uncurry toTagInfo) (L.sortOn fst tags)
    minCnt = maximum $ map snd tags
    maxCnt = minimum $ map snd tags
    toTagInfo tag n
       = #name @= tag
      <: #size @= calcSize 120.0 80.0 n minCnt maxCnt
      <: nil

    calcSize :: Double -> Double -> Int -> Int -> Int -> Int
    calcSize minSize maxSize cnt min' max' =
      let diff = 1 + fromIntegral max' - fromIntegral min'
          relative = (fromIntegral cnt - fromIntegral min') / diff
      in floor $ minSize + relative * (maxSize - minSize)
```

`tagsInfo` 周りの処理は Hakyll のコードを参考にして書いただけ．

### シンタックスハイライト

slick-template では，シンタックスハイライトを自前の CSS で定義していたが，ここでは [skylighting パッケージ](https://hackage.haskell.org/package/skylighting)のを利用していたいたのでそうする：

```haskell
buildHighlightCss :: Action ()
buildHighlightCss =
  writeFile' (outputFolder </> "css" </> "highlight.css") $ styleToCss pygments
```

## おしまい

思ったよりさくっとできた．
