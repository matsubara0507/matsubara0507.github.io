---
title: Haskell Stack で Stackage に無い GHC を使う
tags: [Haskell]
---

ちょっと前に GHC 9.2.1 がリリースされましたね。
GHC 9.2.1 には、ライト層のユーザーでも使いやすい機能（例えば、`GHC2021` とか `OverloadedRecordDot` とか）がいくつかあり、それを早く試したい！って人が少なくないと思います。
僕もです。

Haskell Stack を利用していると、Stackage に新しいバージョン用のスナップショット（`lts-x.y` とか）が来るまで待つ人が多いと思いますが、実は自分で依存ライブラリの列挙をすれば、好きな GHC を使うことができます！
今回はそのメモ書きです。

## 任意の GHC を利用する

これは簡単。`stack.yaml` の `resolver` に GHC を直接指定するとできる：

```yaml
resolver: ghc-9.2.1
```

この場合、依存パッケージは何一つ登録されていないため、`extra-deps` に全て列挙する必要がある。
`nightly` や `lts` を使いつつ GHC だけ上書きする場合は `compiler` を使う：

```yaml
resolver: nightly-2021-11-19
compiler: ghc-9.2.1
```

また、正式リリース済みの GHC ではなく、ソースコードからコンパイルして使いたい場合は `ghc-git` を使う：

```yaml
resolver: nightly-2021-11-19
compiler: ghc-git-COMMIT-FLAVOUR
```

（[公式サイトにまとまっている](https://docs.haskellstack.org/en/stable/yaml_configuration/#compiler)）


## カスタムスナップショットを作る

さらに、[自由に個人のカスタムスナップショットを公開し再利用することができる](https://docs.haskellstack.org/en/stable/pantry/#snapshots)。
LTS の場合、数多くのパッケージが同時にビルド可能になる必要があるため、なかなか新しい GHC のものがリリースされない。
しかし、自分がよく使うパッケージだけなら可能かもしれない。
[ということで、試しに作ってみた](https://github.com/matsubara0507/stack-snapshots)：


```yaml
resolver: ghc-9.2.1

packages:
- OneTuple-0.3.1
- Only-0.1
...

flags:
  cassava:
    bytestring--lt-0_10_4: false
  transformers-compat:
    five-three: true
```

パッケージのいくつかには GHC のコンパイルフラグを利用して、そのパッケージの依存パッケージのバージョンを分岐しているものがある。
そういうのは `flags` で指定する。
Stackage の場合、[この辺りを見ると参考になる](https://github.com/commercialhaskell/stackage/blob/3c8220a4306f697333df6454dcb29e7e66385fff/build-constraints.yaml#L6837)（ただし、Stackage はスナップショットを直接記述せず、設定ファイルから [curator](https://github.com/commercialhaskell/curator) で自動生成しているが）。

そして、`resolver` で GitHub の RAW リンクを resolver で指定すれば使える：

```yaml
resolver: https://raw.githubusercontent.com/matsubara0507/stack-snapshots/main/snapshot.yaml
```

## おしまい

結構多くのパッケージが既に GHC9.2 対応されてて、割とすんなり遊べる。
ありがたい。
