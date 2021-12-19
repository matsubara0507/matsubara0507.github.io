---
title: Bazel でも GHC 9.2.1 でビルドがしたい
tags: [Haskell, Bazel]
---

本記事は 「[Haskell Advent Calendar 2021](https://qiita.com/advent-calendar/2021/haskell)」の4日目の記事です。
空いてたので埋めました。

#

[Bazel](https://bazel.build/)というビルドツールがあります。Bazelでは、いわゆるライブラリのようなものを使うことで、簡単にさまざまなプログラムのビルドやテストの実行を同じインターフェースで行うことができます。例えば：

- [bazelbuild/rules_go](https://github.com/bazelbuild/rules_go) : Goプログラムのビルドやテストなど
- [bazelbuild/rules_docker](https://github.com/bazelbuild/rules_docker) : Dockerイメージのビルドやプッシュなど
- [bazelbuild/rules_nodejs](https://github.com/bazelbuild/rules_nodejs) : Nodeプログラムのビルドやテストなど

そして、[tweag/rules_haskell](https://github.com/tweag/rules_haskell) を使うことで、Bazel を利用して Haskell プログラムのビルドが行えます。今回は今年でた新しいバージョンの Haskell 処理系、GHC 9.2.1 を Bazel を利用してビルドする、までに頑張ったことのメモ書きです。

ちなみに、頑張って修正したので、私のフォークを利用すればとりあえずビルドすることができます。

#

例として [matsubara0507/example-haskell-with-bazel](https://github.com/matsubara0507/example-haskell-with-bazel) というリポジトリも用意しました。CI/CD をいい感じにして、無事、直近3バージョンと各OSのビルドが通るのを確認できました：

![](/assets/fix-rules_haskell-for-ghc_9_2_1/all-green.png)

## rules_haskell の振る舞い

rules_haskell で、どの GHC に対応している（ビルドできるとは言ってない）かは `haskell/gen_bindist.bzl` の `GHC_BINDIST` 変数を見ればわかります：

```py
GHC_BINDIST = \
    {
        ...
        "9.2.1": {
            "darwin_amd64": (
                "https://downloads.haskell.org/~ghc/9.2.1/ghc-9.2.1-x86_64-apple-darwin.tar.xz",
                "c527700a210306098ce85d2c956089deea539aefe1d1816701d5c14cf9c113b7",
            ),
            "linux_amd64": (
                "https://downloads.haskell.org/~ghc/9.2.1/ghc-9.2.1-x86_64-deb9-linux.tar.xz",
                "f09133ed735e9f3b221b5ed54787e5651f039ed0f7dab0ab834a27c8ca68fc9b",
            ),
            "windows_amd64": (
                "https://downloads.haskell.org/~ghc/9.2.1/ghc-9.2.1-x86_64-unknown-mingw32.tar.xz",
                "649e04abd4fa35796070b35de1c353721507a49842b18663aa1c7adc6b4115d8",
            ),
        },
    }
```

`GHC_BINDIST` の URL から `ghc` や `ghc-pkg` などのビルド済みバイナリと標準パッケージなどが固まった `tar.xz` ファイルが手に入ります。`rules_haskell` は、これを Bazel サンドボックス用に展開して利用します。

Linux と macOS の場合は `configure` と `make` を使ってバイナリファイルや haddock ファイルなど様々なファイルを、特定のディレクトリ構成で配置し直したりしているようです。Windows の場合は、`make` が動作しないためか、そのまま使っています。

この `make` 時の振る舞いが 8.10・9.0・9.1 で微妙に異なるため苦労しました。

## バージョン間の違い

以下のような違いがあり、そのまんまビルドすることができませんでした：

1. 展開後のディレクトリ名が違う
2. `html/libraries` 配下のディレクトリの命名規則が違う
3. `package.conf.d` ディレクトリのパスが違う
4. Cabal ファイルの変数が `$topdir` から `${pkgroot}` に変わった
5. ` realpath` コマンドに依存している問題（Mac）
6. Cabal 3.6 から `relocatable` フラグが機能しなくなった
7. `html/libraries` へのパスが間違っている（Windows）

流石に GHC 側の経緯までは完璧に追えてませんが、ひたすらトライアンドエラーして直しました。
一応、[雑な英語で本家に PR を投げました](https://github.com/tweag/rules_haskell/pull/1666)が、マージされるかはわかりません。

### 1. 展開後のディレクトリ名が違う

今までは、どの OS であれ `ghc-X.Y.Z` のようなディレクトリ名でした。しかし、9.2.1 の Windows と Mac、9.0.1 の Windows は（なぜか）`ghc-9.2.1-x86_64-apple-darwin` のような `tar.xz` ファイル名になっていました。仕方がないので、GHC のバージョンと OS によってディレクトリ名が変わるように修正しました：

```py
GHC_BINDIST_STRIP_PREFIX = \
    {
        "9.2.1": {
            "darwin_amd64": "ghc-9.2.1-x86_64-apple-darwin",
            "windows_amd64": "ghc-9.2.1-x86_64-unknown-mingw32",
        },
        "9.0.1": {
            "windows_amd64": "ghc-9.0.1-x86_64-unknown-mingw32",
        },
    }
...
    stripPrefix = "ghc-" + version
    if GHC_BINDIST_STRIP_PREFIX.get(version) != None and GHC_BINDIST_STRIP_PREFIX[version].get(target) != None:
        stripPrefix = GHC_BINDIST_STRIP_PREFIX[version][target]

    ctx.download_and_extract(
        url = url,
        output = unpack_dir,
        sha256 = sha256,
        type = "tar.xz",
        stripPrefix = stripPrefix,
    )
```

### 2. `html/libraries` 配下のディレクトリの命名規則が違う

`haddock` を生成するためか、展開したファイル群の中から標準パッケージの生成済み haddock ファイル置き場を特定する必要があります。8.10 系までは `base-4.14.3.0` のようにバージョンがディレクトリ名に含まれていたため、次のように特定していました：

```py
        # Find a file matching `html/libraries/base-*.*.*.*/*` and infer `docdir` from its path.
        # `GHC.Paths.docdir` reports paths such as `.../doc/html/libraries/base-4.13.0.0`.
        for f in docdir:
            html_start = f.path.find("html/libraries/base-")
            if html_start != -1:
                base_end = f.path.find("/", html_start + len("html/libraries/base-"))
                if base_end != -1:
                    docdir_path = f.path[:base_end]
                    break
```

しかし、9.0 系以降は（なぜか）`base` のようにバージョンを含まなくなりました。なので、`-` の部分を消して対応しました。

### 3. `package.conf.d` ディレクトリのパスが違う

`make` 後に `package.conf.d` までのパスを特定する必要があります（これを `pkgdb` と呼んでるっぽいです）。`package.conf.d` 配下には Cabal ファイルのようなものが置いてあります、rules_haskell では、このファイルを元に haddock ファイルなど各種組み込みパッケージの構成を解決します。macOS のこのパスが、9.0 まで `lib` だったのに対し、9.2 からは `lib/lib` に（なぜか）なっていました。なので、これまた OS とバージョンで切り替えれるように書き換えました：

```py
GHC_BINDIST_LIBDIR = \
    {
        "9.2.1": {
            "darwin_amd64": "lib/lib",
        },
    }
...
    libdir = "lib"
    if GHC_BINDIST_LIBDIR.get(version) != None and GHC_BINDIST_LIBDIR[version].get(target) != None:
        libdir = GHC_BINDIST_LIBDIR[version][target]

    toolchain_libraries = pkgdb_to_bzl(ctx, filepaths, libdir)
```

### 4. Cabal ファイルの変数が `$topdir` から `${pkgroot}` に変わった

前述した `package.conf.d` 配下の Cabal ファイルっぽいのには `$topdir` という変数が含まれていました。rules_haskell では、この変数をいい感じに置換して利用していましたが、9.0 からは `${pkgroot}` に（なぜか）変わっていました。仕方がないので、どっちでもいい感じに置換するように書き換えました：

```py
def path_to_label(path, pkgroot):
    """Substitute one pkgroot for another relative one to obtain a label."""
    if path.find("${pkgroot}") != -1:
        return os.path.normpath(path.strip("\"").replace("${pkgroot}", topdir)).replace('\\', '/')

    topdir_relative_path = path.replace(pkgroot, "$topdir")
    if topdir_relative_path.find("$topdir") != -1:
        return os.path.normpath(topdir_relative_path.replace("$topdir", topdir)).replace('\\', '/')
```

また、macOS の 9.2.1 では `make` を利用して内部でも置換をしている場所があります：

```mk
define patchpackageconf
	cat '$2' | sed 's|haddock-interfaces.*|haddock-interfaces: "$${pkgroot}/$4/html/libraries/$1/$1.haddock"|' \
	         | sed 's|haddock-html.*|haddock-html: "$${pkgroot}/$4/html/libraries/$1"|' \
		 | sed 's|    $${pkgroot}/../../docs/html/.*||' \
	       > '$2.copy'
```

ここ、何故か `"` を前後に追加しているんですよね。Bazel で利用するパスにする場合、邪魔になるので `path.strip("\"")` で除去しています。

### 5. ` realpath` コマンドに依存している問題（Mac）

ちょうど前述した `patchpackageconf` の引数に `pkgroot` から `doc` への相対パスを `$4` として渡しています。この相対パスの解決に `realpath` コマンドを利用していたのですが、これは macOS に標準で入っていません。そのため、`make` を実行すると落ちます。

[これはすでに GHC 側で Issue があり、対応の差分が master へ取り込まれています](https://gitlab.haskell.org/ghc/ghc/-/commit/fab2579e63bb317d4c266d7b949cf96ad6e5d17b)。9.2.2 でもリリースされていれば、それで解決ですが、まだ無いのでパッチを手動で当てることにしました：

```py
    if target == "darwin_amd64":
        patches = {
            # Patch for https://gitlab.haskell.org/ghc/ghc/-/issues/19963
            "9.2.1": ["@rules_haskell//haskell:assets/ghc_9_2_1_mac.patch"],
        }.get(version)
    ...

    # We apply some patches, if needed.
    patch_args = list(ctx.attr.patch_args)
    if unpack_dir:
        patch_args.extend(["-d", unpack_dir])
    patch(ctx, patch_args = patch_args)
```

`unpack_dir` というのが `GHC_BINDIST` の URL から落としてきたのを展開したディレクトリへのパスです。

`relpath.sh` はパッチだけでは実行できないので、事前に用意しておいて呼び出せるようにしておきます：

```py
        if version == "9.2.1":
            ctx.file("{}/mk/relpath.sh".format(unpack_dir), ctx.read(ctx.path(ctx.attr._relpath_script)), executable = False, legacy_utf8 = False)
            execute_or_fail_loudly(ctx, ["chmod", "+x", "mk/relpath.sh"], working_directory = unpack_dir)
```
### 6. Cabal 3.6 から `relocatable` フラグが機能しなくなった

ここはあまりよくわかってないのですが、依存パッケージ（組み込みに限らず）の `Path_xxx` モジュールをビルドしようとすると次のようなエラーで落ちます：

```
ERROR: /path/to/external/stackage/BUILD.bazel:821:22: HaskellCabalLibrary @stackage//:unliftio-core failed: (Exit 1): cabal_wrapper failed: error executing command bazel-out/host/bin/external/rules_haskell/haskell/cabal_wrapper bazel-out/darwin-fastbuild/bin/external/stackage/unliftio-core_cabal_wrapper_args.json

Use --sandbox_debug to see verbose messages from the sandbox

../../../bazel-out/darwin-fastbuild/bin/external/stackage/unliftio-core-0.2.0.1/unliftio-core-0.2.0.1/build/autogen/Paths_unliftio_core.hs:47:17: error:
    Variable not in scope: splitFileName :: FilePath -> (a, b0)
   |
47 |   let (dir,_) = splitFileName exePath
   |                 ^^^^^^^^^^^^^

../../../bazel-out/darwin-fastbuild/bin/external/stackage/unliftio-core-0.2.0.1/unliftio-core-0.2.0.1/build/autogen/Paths_unliftio_core.hs:48:16: error:
    Variable not in scope: minusFileName :: t0 -> String -> String
   |
48 |   return ((dir `minusFileName` "bin") `joinFileName` dirRel)
   |                ^^^^^^^^^^^^^^^
```

`Path_xxx` モジュールは Cabal からビルドしています。[この挙動が Cabal 3.6 から変わっているっぽく](https://github.com/haskell/cabal/pull/6984)、以前は `--enable-relocatable` オプションを指定することで `splitFileName` 関数などを利用して relocate (?) をやっていたようです。しかし、Cabal 3.6 からは依存パッケージに対して relocate ができなくなったぽく、依存パッケージなのに `--enable-relocatable` オプションを指定すると上記のようなエラーが出るようです（わかりにくい）。

仕方がないので、GHC 9.2.1 以降の場合は `--enable-relocatable` オプションを指定しないようにしました：

```py
    enable_relocatable_flags = []
    if not is_windows and json_args["ghc_version"] != None and json_args["ghc_version"] < [9,2,1]:
        # ToDo: not work relocatable from Cabal-3.6.0.0 buildin GHC 9.2.1
        enable_relocatable_flags = ["--enable-relocatable"]
```

理想的には Cabal のバージョンで分岐したいのですが、Cabal のバージョンを簡単に手に入れる方法が思いつかなったので GHC のバージョンでとりあえず分岐しています。

### 7. `html/libraries` へのパスが間違っている（Windows）

Windows は、`make` などをせず、割とそのまま展開したファイルを利用します。`package.conf.d` ディレクトリ配下の Cabal ファイル的なのに `${pkgroot}/../../docs` とあるのですが、他の OS の動作と合わせると  `${pkgroot}/../docs` が正しいはずです。仕方がないので、(5) のときと同じようにパッチを当てて解決しました。

また、GHC 8.10 以前は `html/libraries` へのパスが `doc` だったのに対して、Windows だけは `docs` になっています。
rules_haskell は `doc` 前提になっていたので、よしなに分岐できるようにしました：

```py
GHC_BINDIST_DOCDIR = \
    {
        "9.2.1": {
            "windows_amd64": "docs",
        },
        "9.0.1": {
            "windows_amd64": "docs",
        },
    }
  ...

    docdir = "doc"
    if GHC_BINDIST_DOCDIR.get(version) != None and GHC_BINDIST_DOCDIR[version].get(target) != None:
        docdir = GHC_BINDIST_DOCDIR[version][target]
```

## おまけ：`allow-newer` オプション

Stack には `allow-newer` オプションというめちゃくちゃ便利な機能があります。これをオンにすると、依存パッケージ間のバージョンの上限を全部無視してくれるのです（ビルドできるかは別として）。

このオプションはてっきり Cabal 側にそう言うのがあり、Stack 側はそれへ橋渡しするだけなのかと思ったのですが、実は違いました。Cabal は `--dependency` オプションに全ての依存パッケージを明示することで、バージョンの上限下限のチェックをしないようです。Stack 側は、常にそれを利用し、バージョンの上限下限のチェックは自前で用意していました（`allow-newer` オプションは Stack 独自のチェックを無視するだけ）。

昔は Cabal にも `allow-newer` オプションがあったっぽいですが、上記のように代替可能（無視したければ指定しろ）なので消されたようです：

- [RFC remove allow-older and allow-newer support from Setup.hs · Issue #3581 · haskell/cabal](https://github.com/haskell/cabal/issues/3581)

そのため、Bazel 側で簡単にチェックを外すのは厳しい（Stack と同じチェックの仕組みを再実装する必要がある）のでした。

## おしまい

1週間もかかった笑
