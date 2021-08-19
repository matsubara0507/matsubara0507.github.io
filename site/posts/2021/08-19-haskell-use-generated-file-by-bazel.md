---
title: Bazel で生成したファイルを Haskell から参照する
tags: [Haskell, Bazel, Docker]
---

[前に Elm のコードを Bazel でビルドするためのルールを作りました](https://matsubara0507.github.io/posts/2021-01-18-create-rules_elm.html)．
この生成物を Bazel でビルドしてる Haskell アプリケーションから参照する方法のメモ書きです．

## Bazel生成物を参照する方法

思い付いた方法は2つ：

1. Bazel で Docker イメージを作るときに含めて動的に参照する
2. Bazel で Haskell をビルドするときに埋め込む（Template Haskell）

前者は Docker で固めるだけなので，Haskell 側で特別なことをする必要がなく簡単．
後者は rules_haskell と Haskell 側で試行錯誤する必要があるが，Haskell アプリケーション単体で完結するので便利だ．

### Docker イメージに含める

Bazel を利用して Docker イメージを作る場合は次のように書く：

```python
pkg_tar(
    name = "bin",
    srcs = [":app"],
    mode = "0755",
    package_dir = "/usr/local/bin",
)

container_image(
    name = "image",
    base = "@haskell_base//image",
    tars = [":bin"],
    entrypoint = ["/usr/local/bin/app"],
)
```

`:app` は，例えば rules_haskell の `haskell_binary` などで生成した実行ファイル（の Bazel 生成物）だ．
`container_image` にファイルを直接渡す場合は `files` 属性を利用する：

```python
container_image(
    name = "image",
    base = "@haskell_base//image",
    tars = [":bin"],
    files = [":mainjs"], # ココ
    entrypoint = ["/usr/local/bin/app"],
)
```

`:mainjs` は，例えば rules_elm の `elm_make` などで生成したファイルだ．
この場合，ルートディレクトリ直下に生成物が置かれる．
任意のパスにしたい場合は `directory` 属性を使えば良い：

```python
container_image(
    name = "image",
    base = "@haskell_base//image",
    tars = [":bin"],
    files = [":mainjs"],
    directory = "/work/static", # ココ
    workdir = "/work",
    entrypoint = ["/usr/local/bin/app"],
)
```

しかし，この場合 `:bin` のパスも変わってしまう（今回の場合は `/work/static/usr/local/bin/app` になってしまう）．
そこで，`:mainjs` も `:bin` のように `pkg_tar` を介すようにすれば良い：

```python
pkg_tar(
    name = "static",
    srcs = [":mainjs"],
    mode = "0444",
    package_dir = "/work/static"
)

container_image(
    name = "image",
    base = "@haskell_base//image",
    tars = [":bin", ":static"],
    workdir = "/work",
    entrypoint = ["/usr/local/bin/app"],
)
```

これで実行ファイルは `/usr/local/bin` にあって，Haskell アプリケーション側で読み込む静的ファイルは `/work/static` にあるようにできた．
ただし，こっちの方法の問題として Docker イメージまで作らないと手元で動作確認ができない点がある．
それでは不便な場合は，次の実行ファイル自体に静的ファイルを埋め込んでしまう方法をとると良い．

### Haskell に埋め込む

Haskell でコードを埋め込むには Template Haskell を利用する．
ちなみに，[実際の作業PRはこちら](https://github.com/matsubara0507/homelyapp/pull/4)．

今回は [file-embed パッケージ](https://hackage.haskell.org/package/file-embed-0.0.14.0)を利用する：

```haskell
import Data.FileEmbed (embedDir)
import Servant

type API = "static" :> Raw

server :: ServerT API (RIO Env)
server = serveDirectoryEmbedded $(embedDir "./static")
```

rules_haskell で Template Haskell などのために Haskell のソースコード以外を渡す場合には `extra_srcs` 属性を利用する（[参照](https://github.com/tweag/rules_haskell/tree/b6242a0938323ee741e1dfa1a954d3f4360f1731/tests/extra-source-files)）：

```py
haskell_library(
    name = "homelyapp-library",
    src_strip_prefix = "src",
    srcs = glob(["src/**/*.hs"]),
    deps = [
        "@stackage//:base",
        ... 
    ],
    extra_srcs = [":mainjs"], # ココ
    compiler_flags = GHC_FLAGS,
)
```

`extra_srcs` に渡しているのが Bazel の生成物でない場合はこれでうまくいくが，Bazel の生成物を渡した場合はこれだけではうまくいかない．
というのも，Bazel の生成物のパスが `"./static"` ではないからだ．
`:mainjs` 自体は `./static/main.js` という設定で生成したが，Bazel サンドボックスにおいてはカレントパスが変わってしまう（[Bazelサンドボックスと各種ディレクトリ構造について](https://docs.bazel.build/versions/main/output_directories.html)）．

ではどうすれば良いか．
一応，パスの類推は可能だが，Haskellコード側に Bazel 専用のパスをハードコードするのはいやだ．
で，[いろいろと Issue を漁っていたらドンピシャなものを見つけた](https://github.com/tweag/rules_haskell/issues/1337#issuecomment-632588864)．
Issue のコメント曰く，次のようにしてあげれば良い：

```py
haskell_library(
    name = "homelyapp-library",
    src_strip_prefix = "src",
    srcs = glob(["src/**/*.hs"]),
    deps = [
        "@stackage//:base",
        ... 
    ],
    extra_srcs = [":mainjs"], 
    compiler_flags = [
        "-DMAINJS_FILE=\"$(execpath :mainjs)\"",  # ココ
    ] + GHC_FLAGS,
)
```

[`-D` オプションはCプリプロセッサによる変数を外部から与えるやつ](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html#c-pre-processor-options)で，`MAINJS_FILE` 変数に `execpath :mainjs` の結果を与えている．
[`execpath` は Bazel 専用の特殊な変数展開らしく](https://docs.bazel.build/versions/4.1.0/be/make-variables.html#predefined_label_variables)，サンドボックスのルートから与えた Bazel 生成物への相対パスを返す．
つまり，これを `embedDir` に渡してやれば良い：

```haskell
{-# LANGUAGE CPP #-}

server :: ServerT API (RIO Env)
server = serveDirectoryEmbedded $(embedDir (takeDirectory MAINJS_FILE))
```

## おまけ：Hazell の修正

`compiler_flags` をいじった結果，Hazell が動作しなくなったので直した．
問題箇所は2つ：

1. リストの結合（`+`）は未対応
2. 文字列の `"` のエスケープが未対応

両方対応した：

- [Support append list operator and escaped double quote in string literal by matsubara0507 · Pull Request #2 · matsubara0507/hazell](https://github.com/matsubara0507/hazell/pull/2)

リスト結合はだいぶ雑に実装しており，もし別の式をパースしたくなった場合はまるっと書き直す必要がある（演算子の優先順位などを考慮していないため）．

## おしまい

ちなみに，僕は後者の埋め込みを利用することにしました．
こっちだと，`bazel run` のワンコマンドでアプリケーションをローカルで起動できるからです．
