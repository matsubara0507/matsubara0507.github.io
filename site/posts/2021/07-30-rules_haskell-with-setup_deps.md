---
title: Bazel で Haskell の Custom Setup をする
tags: [Bazel, Haskell]
---

Bazel を使って Haskell アプリケーションをビルドしてるんですけど，依存パッケージが Custom Setup を使ってるときにちょっと躓いたので，そのメモ書きです．

## Bazel で Custom Setup

[mdium](https://github.com/matsubara0507/mdium) という CLI ツールを実験的に Bazel でビルドをしている．
LTS を 18.0 にあげようと依存パッケージをアップデートしたところ，次のようなエラーが出た：

```
/private/var/tmp/.../sandbox/darwin-sandbox/10/execroot/mdium/external/stackage/xml-conduit-1.9.1.1/Setup.hs:3:1: error:
    Could not find module ‘Distribution.Extra.Doctest’
    Perhaps you meant Distribution.Simple.Doctest (from Cabal-3.2.1.0)
    Use -v (or `:set -v` in ghci) to see a list of the files searched for.
  |
3 | import Distribution.Extra.Doctest (defaultMainWithDoctests)
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

Stack でビルドした場合には起きないエラーだ．
エラーの内容はどうやら，Cabal の Custom Setup で利用している依存パッケージが足りていないようだ（Custom Setup については「[cabal build で package.yaml を使う - BIGMOON Haskeller's BLOG](https://haskell.e-bigmoon.com/posts/2018/12-25-cabal-preprocessing.html)」が日本語記事では分かりやすい）．

[tweag/rules_haskell](https://github.com/tweag/rules_haskell) で Issue を漁ったところ，[まさに同じの](https://github.com/tweag/rules_haskell/issues/1314)があった．
どうやら `setup_deps` を使えば良いらしい：

```py
stack_snapshot(
    name = "stackage",
    packages = [
        "aeson",
        "base",
        "dotenv",
        "extensible",
        "fallible",
        "github",
        "mix",
        "mix-json-logger",
        "mix-plugin-github",
        "pandoc",
        "pandoc-types",
        "rio",
        "wreq",
    ],
    setup_deps = {
        "xml-conduit": ["cabal-doctest"], # 追記
    },
    local_snapshot = "//:stack-snapshot.yaml",
)
```

うまくビルドできました．
やったね．

## おまけ：hazell で対応する

ぶっちゃけ，表題の「Bazel で Haskell の Custom Setup をする」に関してはこれでおしまい．
Issue もあるので記事にするほどのことでもない（笑）．

ただ，私は `stack_snapshot` ルールを[自作ツール hazell](https://github.com/matsubara0507/hazell) を使って hpack の package.yaml から生成している．
しかし，残念ながら `setup_deps` には対応していないので対応する必要があった（ここからが本題）．

# 

[作業PRはコチラ](https://github.com/matsubara0507/hazell/pull/1)

### Dict 型を扱えるようにする

実は，今までの機能であれば [Dict 型](https://docs.bazel.build/versions/main/skylark/lib/globals.html#dict)を扱う必要がなかったので未対応だった（WORKSPACE などを読み込むとエラーになる）．
まずはここから対応する．

まずは Bazel ルールの引数の型 `RuleArg` を拡張する：

```haskell
data RuleArg
  = RuleArgString String
  | RuleArgBool Bool
  | RuleArgArray [RuleArg]
  | RuleArgDict (Map String RuleArg) -- 追加
  | RuleArgConst String
  | RuleArgGlob String
```

`Map` を使うと，元のキーの順番が保持されないが，まぁ本質的には困らないのでとりあえず `Map` にした．
フォーマットするときにモヤモヤしだしたらキーの順番が保持される仕組みを考えることとする．

次にパーサーを追加：

```haskell
buildRuleArgParser :: Parser RuleArg
buildRuleArgParser
    = buildRuleArgArrayParser
  <|> buildRuleArgDictParser  -- 追加
  <|> buildRuleArgBoolParser
   ...

-- `{ "key" : value, "key" : value }` って感じのをパースしたい
buildRuleArgDictParser :: Parser RuleArg
buildRuleArgDictParser = do
  char '{'
  space
  dict <- buildRuleDictMemberParser `sepAndEndBy` (comma, space >> char '}')
  pure $ RuleArgDict (Map.fromList dict)
  where
    buildRuleDictMemberParser :: Parser (String, RuleArg)
    buildRuleDictMemberParser = do
      key <- stringLitParser
      space
      char ':'
      space
      val <- buildRuleArgParser
      pure (key, val)
```

`sepAndEndBy` コンビネーターは自分で定義しているやつで，名前の通りセパレーターと終端に使うパーサーをそれぞれ指定して繰り返し処理させる．

最後にプリティプリンターを定義して完成：

```haskell
instance Pretty RuleArg where
  pretty (RuleArgString str)  = fromString (show str)
  pretty (RuleArgBool True)   = "True"
  ...
  pretty (RuleArgDict dict)   = pretteyDict dict -- 追加
  ...

-- 要素がある場合は次のように出力したい：
--  hoge = {
--    "key1": value1,
--    "key2": value2,
--  }
pretteyDict :: Map String RuleArg -> Doc ann
pretteyDict dict =
  if Map.null dict then
    "{}"
  else
    vsep [nest 4 $ vsep ("{" : pretteyDict' dict), "}"]
  where
    pretteyDict' = map (\(k, v) -> fromString (show k) <> ": " <> pretty v <> ",") . Map.toList
```

これで hazell に修正した WORKSPACE ファイルを食わしてもパースエラーが起きなくなった．
しかし，追記した `setup_deps` が消えてしまう．
困った．

### 未定義の引数は残すようにする

実は，元の `stack_snapshot` と新しく生成した `stack_snapshot` のマージが雑すぎて，新しく生成した `stack_snapshot` にない引数は消してしまうようになっていた．
なので，元の `stack_snapshot` にある引数はちゃんと保持するようにマージ処理を書き直した：

```haskell
import qualified Data.Map.Merge.Strict as Map

mergeRuleArgs :: BuildContent -> Rule -> BuildContent
mergeRuleArgs (BuildRule name args) rule =
  BuildRule name . Map.toList $ Map.merge
    Map.preserveMissing
    Map.preserveMissing
    (Map.zipWithMatched $ \_ old new -> new)
    (Map.fromList args)
    (Map.fromList $ ruleArgs rule)
mergeRuleArgs content _ = content
```

1引数目が WORKSPACE ファイルをパースして得た元々の `stack_snapshot` ルールで，2引数目が package.yaml から生成した新しい `stack_snapshot` ルール（型が違うのはお気になさらず）．
連想配列の結合にはどうやら [`Data.Map.Merge` の `merge` 関数](https://hackage.haskell.org/package/containers-0.6.5.1/docs/Data-Map-Merge-Strict.html#v:merge)を利用すると良いらしい（もちろん，もっとパフォーマンスの良いサードパーティパッケージはあるだろうが）．
`merge` 関数は3つの関数を受け取る高階関数である．
型がぱっと見謎（`SimpleWhenMissing k a c` など）だが，要するに次の3つのケースに関する関数を要求している（`merge m1 m2`）：

1. `m1` だけ要素があった場合の `key -> m1Value -> Maybe newValue` な関数
2. `m2` だけ要素があった場合の `key -> m2Value -> Maybe newValue` な関数
3. 両方に要素があった場合の `key -> m1Value -> m2Value -> Maybe newValue` な関数

全て最後が `Maybe` なのは，`Nothing` の場合はキーそのものを消すためだ．
今回は (1)(2) はヒットした方をそのまま利用し，(3) の場合は新しい方（`m2`）優先にしたい．
ヒットしたのをそのまま使う場合は `preserveMissing` 関数を使えばよい．
`zipWithMatched` 関数は，前述した `Nothing` のケースを排除した `zip` 関数のようなものだ．

### Cabal ファイルを集める

正直，機能的にはここまでで十分だったが，せっかくなので `setup_deps` も自動生成する方法を実装してみる．
Custom Setup の依存パッケージは Cabal ファイルの `custom-setup` の `setup-depends` に書いてある．
つまり，まずはインダイレクトも含む全ての依存パッケージの Cabal ファイルを集める必要がある（これはなかなか大変）．

現在解析している package.yaml には直接の依存パッケージしか書いてないので依存パッケージの依存パッケージなども含めて列挙する方法を考える．
いろいろ試行錯誤した結果，とりあえずは `stack ls` を使うことにした（Stack も Haskell 製なので，いずれ直接扱えるようにしたい）：

```haskell
import RIO.Process (HasProcessContext, proc, readProcessStdout_, withWorkingDir)

runStackLs
  :: (HasProcessContext env, HasLogFunc env, MonadReader env m, MonadIO m, HasCallStack)
  => FilePath -> m [DotPayload]
runStackLs path = do
  out <- proc "stack" ["ls", "dependencies", "json", "--test"] (withWorkingDir path . readProcessStdout_)
  case JSON.eitherDecode out of
    Left e  -> logError (displayShow e) >> pure mempty
    Right a -> pure a
```

rio を利用して外部コマンドを安全に呼び出すには [`RIO.Process` モジュール](https://hackage.haskell.org/package/rio-0.1.20.0/docs/RIO-Process.html)を使うのだが少しクセがある．
基本は `HasProcessContext m` 型クラス配下のモナド `m` で [`proc` 関数](https://hackage.haskell.org/package/rio-0.1.20.0/docs/RIO-Process.html#v:proc)を呼べば良い：

```haskell
proc
  :: (HasProcessContext env, HasLogFunc env, MonadReader env m, MonadIO m, HasCallStack)	 
  => FilePath	
  -> [String]	
  -> (ProcessConfig () () () -> m a)	 
  -> m a
```

1引数目と2引数目が呼び出したいコマンドと与える引数なのだが，問題は3引数目．
これは，コマンドを呼び出すプロセスの設定を関数結合な感じで定義している．
例えば今回は，返り値として標準出力が欲しいので `readProcessStdout_` を使い，加えて実行ディレクトリを変えたいので `withWorkingDir` も呼んでいる．
他にも Exit Code を返り値にしたり，標準入力を与えたりするコンビネーターが存在する．

で，`stack ls` の結果は JSON 形式で出力してパースしている．
`DotPayload` 型は [Stack のコード](https://github.com/commercialhaskell/stack/blob/v2.7.1/src/Stack/Dot.hs)を呼んで必要なものだけ切り出した型だ．
あとは `DotPayload` から Cabal ファイルを拾ってくる関数を定義するだけ：

```haskell
type CabalPackage = Cabal.PackageDescription

readCabalFile :: MonadIO m => DotPayload -> m (Maybe (Either String CabalPackage))
readCabalFile payload =
  -- hackage 以外の場合はいったん未対応
  case payloadLocation payload of
    (Just (PackageLocation "hackage" url)) -> do
      Just . (parsePackageDescription =<<) <$> get (Text.unpack url ++ "/" ++ packageName ++ ".cabal")
    _ ->
      pure Nothing
  where
    packageName = Text.unpack (payloadName payload)
    parsePackageDescription b = case Cabal.parseGenericPackageDescriptionMaybe b of
      Nothing -> Left $ "cannnot parse to cabal file " ++ packageName
      Just p  -> Right $ Cabal.packageDescription p
```

Cabal ファイルは Hackage から引くことができる（例えば `https://hackage.haskell.org/package/rio-0.1.20.0/rio.cabal` など）．
`get` 関数は req パッケージを使って自分で定義した，いい感じにしただけの GET HTTP リクエストするだけの関数（割愛）で，これを使って取ってきている．
[Cabal ファイルのパーサーは Cabal パッケージにある](https://hackage.haskell.org/package/Cabal-3.4.0.0/docs/Distribution-PackageDescription-Parsec.html#v:parseGenericPackageDescriptionMaybe)のでそれを利用しているだけだ．

### `setup_deps` を生成する

あとは集めた Cabal ファイルを使って `setup_deps` を構築するだけだ．
Cabal パッケージの関数をそのまま呼ぶことで Custom Setup 用の依存パッケージは集めることができる：

```haskell
import qualified Distribution.Types.Dependency  as Cabal
import qualified Distribution.Types.PackageId   as Cabal
import qualified Distribution.Types.PackageName as Cabal

toSetupDeps :: CabalPackage -> [String]
toSetupDeps =
  fmap (Cabal.unPackageName . Cabal.depPkgName) . maybe [] Cabal.setupDepends . Cabal.setupBuildInfo

toPackageName :: CabalPackage -> String
toPackageName =
  Cabal.unPackageName . Cabal.pkgName . Cabal.package
```

これを雑に `setup_deps` にしたところ，Cabal パッケージなど不要なのが大量に出てきてしまった．
おそらく，[GHC にデフォルト含まれるパッケージ](https://downloads.haskell.org/~ghc/8.10.4/docs/html/users_guide/packages.html#using-packages) は `setup_deps` に指定する必要がない気がするので弾くようにした：

```haskell
buildSetupDeps :: [CabalPackage] -> [(Maybe [Char], RuleArg)]
buildSetupDeps cabals =
  case [(Cabal.toPackageName c, RuleArgArray ds) | c <- cabals, let ds = toSetupDepsArg c, not (null ds)] of
    [] ->
      []
    deps ->
      [(Just "setup_deps", RuleArgDict $ Map.fromList deps)]
  where
    toSetupDepsArg = fmap RuleArgString . filter (not . (`elem` ghcPkgs)) . Cabal.toSetupDeps

ghcPkgs :: [String]
ghcPkgs = ["Cabal", ... ]
```

これで無事完成．

## おしまい

ちなみに，全ての Cabal ファイルを集めて `setup_deps` を構築するのはそこそこ時間がかかるため，するかしないかをフラグで制御できるようにしてる．
Hackage にある Cabal ファイルはパッケージのバージョン毎に（たぶん）不変なのでローカルにキャッシュする方法を用意したいね．
