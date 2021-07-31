---
title: rules_elm で依存パッケージのインストールをキャッシュする
tags: [Elm, Bazel]
---

前に，Elm 用の Bazel ルールがないので作りました：

- [rules_elm を作る - ひげメモ](https://matsubara0507.github.io/posts/2021-01-18-create-rules_elm.html)

しかしこれは依存パッケージのDLなどをうまくキャッシュしたりしないので毎回フルビルドが走ってしまいます．
今回は，この問題をなんとか解決したという話です．

## Elm の依存パッケージのキャッシュ

まずはそもそも，Elm は依存パッケージをどうやってローカルにキャッシュしているかについて紹介する．
`elm make` などをすると Elm プロジェクトの配下に `elm-stuff` という（基本的には git 管理しない）ディレクトリができるが，中身を見てみるとここには無い．
Elm コンパイラ（バージョンは 0.19.1）のソースコードを直接呼んだ結果，`ELM_HOME` 環境変数に設定したパスのディレクトリに保存されているようだ．
`ELM_HOME` 環境変数が設定されてない場合は `$HOME/.elm` が使われている．

```
$ ls ~/.elm/0.19.1/packages/
bartavelle		elm			elm-community		elm-explorations	justinmimbs		lock			registry.dat		rtfeldman
```

`registry.dat` ファイルには，このディレクトリ配下で既に管理しているパッケージ群が書かれてある．
`lock` ファイルは，このディレクトリへの書き込みを排他制御するためのもので，[filelock パッケージ](https://hackage.haskell.org/package/filelock)を利用して行っている．
`elm install` や `elm make` を実行すると `lock` ファイルによってロックをとり，`registry.dat` ファイルを見て対象のパッケージがダウンロード済みかを確認し，なければダウンロードしてくるといった感じだ．

### Bazel でどうするか

`registry.dat` ファイルがあるため，依存パッケージ別に保存し再利用することは難しい．
なので，Bazel のサンドボックス内に保存した `ELM_HOME` の中身をまるまるドカッと Bazel の生成物として再利用することにした．
この生成物は elm.json に依存することにすれば，elm.json が変更されない限りは再ダウンロードされないはず．
もちろん，elm.json が少しでも変更されると全て再ダウンロードされるが，そこまで時間かからないので取り敢えず目を瞑ることにする．

## elm_dependencies ルール

それらを行う `elm_dependencies` ルールを作った．
次のように利用する：

```py
elm_dependencies(
    name = "deps",
    elm_json = "elm.json",
)

elm_make(
    name = "index",
    srcs = glob(["**"]),
    elm_json = "elm.json",
    main = "src/Main.elm",
    output = "index.html",
    elm_home = ":deps", # elm_dependencies の生成物をココで指定する
)
```

[作業 PR はこちら](https://github.com/matsubara0507/rules_elm/pull/1)．

### ELM_HOME の中身を保存する

まずは `elm_dependencies` の振る舞いについて．
試行錯誤した結果：

1. Bazel のサンドボックス配下に `ELM_HOME` を指定
2. いったん無理やり `elm make Main.elm` を実行
3. `ELM_HOME` 配下を ZIP で固めてルールの生成物とする

方法をとることにした．
Bazel の生成物にはディレクトリも指定可能だが，生成物をあとで利用する際に ZIP ファイルにしておいた方が都合が良いのでそうしている（詳しくは後述）．

Bazel 内で実行される Elm コマンドは Windows でも動作するように Python を使ってラップされている（rules_haskell を参考にした）．
なので，今回も同様に専用の Python スクリプトを記述することにした：

```python
import json
import os
import os.path
import shutil
import subprocess
import sys

def run(cmd, *args, **kwargs):
    try:
        subprocess.run(cmd, check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE, *args, **kwargs)
    except subprocess.CalledProcessError as err:
        sys.stdout.buffer.write(err.stdout)
        sys.stderr.buffer.write(err.stderr)
        raise

elm_runtime_path = os.path.abspath("@@ELM_RUNTIME@@") # Bazelのテンプレート機能で後から指定
elm_project_root = sys.argv.pop(1) # トップレベル以外で Elm プロジェクトを指定した場合を考慮
os.environ["ELM_HOME"] = os.path.abspath(os.getenv("ELM_HOME")) # 念のため絶対パスにする

os.chdir(elm_project_root)

# elm.json しか無い前提なので `source-directories` で指定してあるパスを生成しておく
elm_json = json.load(open("elm.json"))
if elm_json["type"] == "application":
    for srcdir in elm_json["source-directories"]:
        os.makedirs(srcdir, exist_ok = True)
# Main.elm はなんでも良いのでコンパクトなのを生成
with open("Main.elm", mode = "w") as f:
    f.write("import Browser\nimport Debug\nmain = Browser.sandbox (Debug.todo \"temp\")")

run([elm_runtime_path, "make", "Main.elm"])

# ZIP で固める
elm_home = os.getenv("ELM_HOME")
shutil.make_archive(elm_home, "zip", root_dir = elm_home) 
```

`elm_dependencies` ルールでは elm.json しか Bazel サンドボックスに渡していないので，`elm make Main.elm` するために必要なファイルやディレクトリを生成する必要がある．
アプリケーションタイプの elm.json の場合，`source-directories` で指定したパスが存在しないとエラーになるので mkdir しておく（パッケージタイプについては，今回はまだ未対応）．
また，`elm make` するには `Main` ファイルを指定しないといけないので適当なのを生成している．
で，最後に [`shutil.make_archive`](https://docs.python.org/3/library/shutil.html#shutil.make_archive) 関数を利用して ZIP に固めている．

#

あとはこの Python スクリプトを呼び出すルールを作成し，その生成物をZIPファイルとする：

```py
def _elm_dependencies_impl(ctx):
    elm_compiler = ctx.toolchains["@rules_elm//elm:toolchain"].elm
    elm_home = ctx.actions.declare_directory(".elm")
    output = ctx.actions.declare_file(".elm.zip") # ZIPファイルを生成物にする

    ctx.actions.run(
        executable = ctx.executable._elm_wrapper,
        arguments = [ctx.file.elm_json.dirname],
        inputs = [elm_compiler, ctx.file.elm_json],
        outputs = [output, elm_home],
        env = {"ELM_HOME": elm_home.path},
    )
    return [DefaultInfo(files = depset([output]))]

elm_dependencies = rule(
    _elm_dependencies_impl,
    attrs = {
        "elm_json": attr.label(allow_single_file = True),
        "_elm_wrapper": attr.label(
            executable = True,
            cfg = "host",
            default = Label("@rules_elm//elm:elm_dependencies"),
        ),
    },
    toolchains = [
        "@rules_elm//elm:toolchain",
    ]
)
```

### ELM_HOME の中身を展開する

あとは `elm_dependencies` ルールの生成物を `elm_make` ルールで利用できるようにするだけだ．
まずは `elm_make` に引数を追加して生成物を渡せるようにする：

```py
def _elm_make_impl(ctx):
    ...
    env = {}
    inputs = [elm_compiler, ctx.file.elm_json] + ctx.files.srcs
    # elm_home が指定している場合は ZIP ファイルとそのパスをラッパースクリプトに渡す
    if ctx.file.elm_home != None:
        env["ELM_HOME_ZIP"] = ctx.file.elm_home.path
        inputs.append(ctx.file.elm_home)

    ctx.actions.run(
        executable = ctx.executable._elm_wrapper,
        arguments = [
            ctx.file.elm_json.dirname,
            "make", ctx.attr.main, "--output", output_file.path,
        ],
        inputs = [elm_compiler, ctx.file.elm_json] + ctx.files.srcs,
        inputs = inputs,
        outputs = [output_file],
        env = env,
    )
    ...

elm_make = rule(
    ...
    attrs = {
    ...
        # 追加（この引数はなくても良い）
        "elm_home": attr.label(
            allow_single_file = True,
        ),
    ...
)
```

あとは `elm_make` で利用している Elm コマンドのラッパースクリプトを編集する：

```python
import zipfile
...

# ここを追記
if os.getenv("ELM_HOME_ZIP") == None:
    os.putenv("HOME", os.getcwd())
else:
    elm_home = os.getcwd() + "/.elm"
    elm_home_zip = os.getenv("ELM_HOME_ZIP")
    with zipfile.ZipFile(elm_home_zip) as elm_zip:
        elm_zip.extractall(elm_home)
    os.environ["ELM_HOME"] = elm_home

os.chdir(elm_project_root)
...
```

で，なぜ ZIP ファイルを介しているのかというと，Bazel ルールの生成物を別のルールで利用する場合は read only じゃないといけないからだ．
前述したとおり，`elm make` では `lock` ファイルを利用して排他制御をする．
その時，書き込み可能でファイルを open しようとする．
そのため，`ELM_HOME` のディレクトリ自体を直接の生成物にして次のルールに渡すとエラーになってしまう：

```
elm-mac: /private/var/tmp/.../execroot/rules_elm/bazel-out/darwin-fastbuild/bin/examples/build-project/.elm/0.19.1/packages/lock: openFd: permission denied (Permission denied)

-- ERROR -----------------------------------------------------------------------

I ran into something that bypassed the normal error reporting process! I
extracted whatever information I could from the internal error:

>   thread blocked indefinitely in an MVar operation

These errors are usually pretty confusing, so start by asking around on one of
forums listed at https://elm-lang.org/community to see if anyone can get you
unstuck quickly.
...
```

なので，一度 ZIP で固めてサンドボックスに展開するようにすれば乱暴だがうまくいく（他に良い方法があれば教えて欲しい...）．

## おしまい

Haskell 読めるおかげで Elm の振る舞いをささっと調べられるのは便利．
それと，記事を書きながら思いついたが，`ELM_HOME` をプロジェクト直下にして vendoring みたいにするのもありっちゃありですね（Go みたいに）．
