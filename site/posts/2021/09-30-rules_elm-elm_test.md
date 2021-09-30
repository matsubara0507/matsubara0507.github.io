---
title: rules_elm に elm-test するルールを追加する
tags: [Elm, Bazel, Rust]
---

前に、Bazel で Elm をビルドしたりするためのルール群である [matsubara0507/rules_elm](https://github.com/matsubara0507/rules_elm) を作りました。
これには、`elm make` 相当のことをしてくれるルールと、依存パッケージをキャッシュするためのルールしかありませんでした。

今回は、それに Elm のテストを実行するためのルールを追加するためのメモ書きです。
[作業 PR はこちら](https://github.com/matsubara0507/rules_elm/pull/4)。


## elm-test-rs

Elm 本体はテストするためのナニガシを提供していない。
テスト用のパッケージは提供しているが、今のところ実行方法はサードパーティに委ねている。
現状、デファクトスタンダードなのは [rtfeldman/node-test-runner](https://github.com/rtfeldman/node-test-runner) を利用して実行する方法だ。

しかし、こいつは Node 環境で動作する。
不可能ではないが、このためだけに rules_node を利用して Bazel で Node を利用するのは面倒だ。
できればシングルバイナリで提供されてるものを利用したい。

#

一瞬自作するか？と迷ったが、もしかしたらあるかもしれないとググってみたところ、なんとあった：

- [mpizenberg/elm-test-rs - GitHub](https://github.com/mpizenberg/elm-test-rs)

まさかの Rust 製。

### vs. symlink

いつものように toolchains 化し、いざ実行してみたところ次のようなエラーが出た：

```
elm-test-rs 1.1.0 for elm 0.19.1
--------------------------------

Generating the elm.json for the Runner.elm
The dependencies picked to run the tests are:
{
  "direct": {
    "elm/browser": "1.0.2",
    "elm/core": "1.0.5",
    "elm/html": "1.0.0",
    "elm/json": "1.1.3",
    "elm-explorations/test": "1.2.2",
    "mpizenberg/elm-test-runner": "4.0.5"
  },
  "indirect": {
    "elm/random": "1.0.0",
    "elm/time": "1.0.0",
    "elm/url": "1.0.0",
    "elm/virtual-dom": "1.0.2"
  }
}
get_module_name of: /path/to/elm-test-rs/tests/example-projects/passing/app/tests/Tests.elm
Error: This file "/path/to/elm-test-rs/tests/example-projects/passing/app/tests/Tests.elm" matches no source directory! Imports wont work then.
```

elm-test-rs 内で、テスト用の Elm ファイルが設定されてるディレクトリ内に存在するかどうかをチェックするところで落ちている。
もちろん、ファイルは Bazel サンドボックスに渡している。
色々調査した結果、どうやらファイルが symlink されたものの場合、このようにエラーとなってしまうらしい。
ファイルが symlink になってしまうのは Bazel の性質上仕方ないことでどうしようもない。
なので、elm-test-rs 側に修正 PR を投げて対応してもらった：

- [Support Elm project have symlink file by matsubara0507 · Pull Request #93 · mpizenberg/elm-test-rs](https://github.com/mpizenberg/elm-test-rs/pull/93)

無事マージされて新しいバージョン v1.2 がリリースされた。

## vs. Windows

あとは適当にルールを書いて無事動作した。
と思いきや、GitHub Actions で回しているテストで Windows だけが落ちるようになってしまった。

元々 Windows 対策として Shell Script の代わりに Python スクリプトを噛ませていたのにこっちでは少しサボってしまった。
なので、同様に Python を利用してテストをテスト用ルールを作る。
`bazel build` 時に Python スクリプトを実行したいだけだったときは `py_binary` を利用したが、今回はテストをしたいので `py_test` を使う：

```py
def elm_test(name, **kwargs):
    _elm_test_wrapper(
        name = name + ".py",
        src_name = name,
        elm_wrapper_tpl = Label("@rules_elm//elm/private:elm_test_wrapper.py.tpl"),
        **kwargs,
    )
    py_test(
        name = name,
        srcs = [name + ".py"],
        srcs_version = "PY3",
        python_version = "PY3",
    )
```

`_elm_test_wrapper` というのが、テンプレートを元に `bazel test` で実行する Python スクリプトを生成するルールで、`py_test` がそのテスト用のスクリプトを `bazel test` で実行してくれる。
`bazel build` で使っていたスクリプトと異なり、`bazel test` で実行したいナニガシ（正確には `bazel run`）は runfiles と呼ばれる実行時に参照できるファイル群を用意しておく必要がある：

```py
def _elm_test_wrapper_impl(ctx):
    elm_compiler = ctx.toolchains["@rules_elm//elm:toolchain"].elm
    elm_test_bin = ctx.toolchains["@rules_elm//elm:toolchain"].elm_test

    inputs = [
        ctx.toolchains["@rules_elm//elm:toolchain"].elm,
        ctx.toolchains["@rules_elm//elm:toolchain"].elm_test,
        ctx.file.elm_json,
    ] + ctx.files.srcs + ctx.files.tests

    substitutions = {
        "@@ELM_RUNTIME@@": elm_compiler.short_path,
        "@@ELM_TEST@@": elm_test_bin.short_path,
        "@@PROJECT_ROOT@@": ctx.file.elm_json.short_path.rsplit("/", 1)[0],
        "@@ELM_HOME_ZIP@@": "",
    }

    if ctx.file.elm_home != None:
        substitutions["@@ELM_HOME_ZIP@@"] = ctx.file.elm_home.short_path
        inputs.append(ctx.file.elm_home)

    elm_wrapper = ctx.actions.declare_file(ctx.attr.src_name + ".py")
    ctx.actions.expand_template(
        template = ctx.file.elm_wrapper_tpl,
        output = elm_wrapper,
        is_executable = True,
        substitutions = substitutions,
    )
    return [DefaultInfo(
        files = depset([elm_wrapper]), 
        runfiles = ctx.runfiles(files = inputs), # この部分
    )]
```

`bazel build` 用のルールのときは、`py_binary` には Python スクリプトしか含まれておらず、Python スクリプトを別のルールで実行するときに上記のようなファイル群を構築していた。
しかし、`bazel test` は `bazel run` と同じで `bazel build` とは実行環境が少し異なるのだ（実行環境を `bazel build` しているイメージ）。

### elm-test-rs から elm バイナリが見えない

試行錯誤して、やっとこさ Bazel + Windows での問題は突破できたところで elm-test-rs 由来でのエラーが生じた。
Windows だけなぜか、elm-test-rs から elm バイナリが見当たらないというエラーが出たのだ：

```
==================== Test output for //examples/build-project:sample-test:
Error: 
Failed to run C:\path\to\elm. Are you sure it's in your PATH?
If you installed elm locally with npm, maybe try running with npx such as:

    npx --no-install elm-test-rs

Caused by:
    cannot find binary path
...
```

確かに、Bazel サンドボックスで参照できる位置に elm バイナリはある。
仕方ないので elm-test-rs コードを読んで、手元の Windows に Rust 環境を用意して色々動作を追ってみた。
で、結果としては Bazel ツールチェインでインストールした elm バイナリに拡張子が無いのが問題だった。
elm-test-rs は、バイナリの存在確認に [which](https://github.com/harryfei/which-rs/tree/4.0.2) ライブラリを使っているのだが、[これが Windows の場合は特定の拡張子が無いとバイナリとして識別してくれない](https://github.com/harryfei/which-rs/blob/4.0.2/src/finder.rs#L134-L151)。
なので、Windows の場合は `.exe` を付けるように改修した。

## 使う

趣味の Haskell + Elm + Bazel プロジェクトで使ってみた。[作業 PR はこちら](https://github.com/matsubara0507/homelyapp/pull/5)。利用は簡単で、BUILD ファイルに以下のようなのを追記するだけ：

```py
elm_test(
    name = "test-elm",
    tests = glob(["elm-test/**"]),
    srcs = glob(["elm-src/**"]),
    elm_json = "elm.json",
    elm_home = ":deps",
)
```

ここで `:deps` というのは依存パッケージを固めた Bazel 生成物で、毎回依存パッケージのインストールをしないようにするための工夫だ。
これで、`bazel test //...` と実行するだけで Haskell と Elm のテストが同時に実行されるようになった。

## おしまい

