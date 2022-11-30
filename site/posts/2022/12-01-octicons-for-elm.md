---
title: Elm 用の Octicons パッケージを作る
tags: [Elm, Haskell, GitHub]
---

[Octicons](https://primer.style/octicons) というのは、GitHub が提供している SVG アイコン集です。
わたしは、なんかしらの Web アプリを作るときに [Primer CSS](https://primer.style/css/) と Octicons をよく使います。

また、なんかしらの Web アプリを作るときに Elm もよく使います。
[Elm で Octisons を使うためのパッケージが既にあります](https://package.elm-lang.org/packages/capitalist/elm-octicons)が、長いことアップデートされてない（2022年12月現在での最終アップデートは2018年の v2.3.0）ので、いっそのこと自作しましたという話です。

## octicons.elm

出来上がったのがこちら

[og:image style="max-width: 500px;"](https://github.com/matsubara0507/octicons-elm)

Octicons は主に3つのサイズがあるので、サイズごとの別のモジュールを用意した：

```elm
import Html.Attributes exposing (style)
import Octicons.Medium as Octicons -- 16px のやつ

view : Model -> Html msg
view model = 
    ...
    Octicons.alert [ style "fill" "red" ]
```

公式が提供しているライブラリ（Ruby や JavaScript）によっては、自動でサイズを選択してくれるのもがるが、実装が煩雑になりそうだったのでやめた。
動作確認を兼ねて、このパッケージを利用して作った[サンプルページ](https://matsubara0507.github.io/octicons-elm/)も用意した。

## 自動生成

サンプルページや、Octicons のページを見てもらうとわかるが、アイコンの量が非常に多い。
これを1つ1つ手で Elm コードに書き写すのは苦行なので、いい感じに自動生成するプログラムを Haskell で作ることにした。
リポジトリの `svg2elm` ディレクトリ配下がそのプラグラムのプロジェクトだ。

やってることは簡単で、[Octicons のリポジトリ](https://github.com/primer/octicons)の `icons` ディレクトリにある SVG ファイルを読み込んで
、[xml-conduit パッケージ](https://hackage.haskell.org/package/xml-conduit)でパースし、XMLの木構造を再帰的に Elm のコード文字列に変換して書き出すだけ。
こんな感じ：

```elm
alert16Nodes : List (Svg msg)
alert16Nodes =
    [ Svg.path [ Svg.Attributes.d "M8.22 1.754a.25.25 0 00-.44 0L1.698 13.132a.25.25 0 00.22.368h12.164a.25.25 0 00.22-.368L8.22 1.754zm-1.763-.707c.659-1.234 2.427-1.234 3.086 0l6.082 11.378A1.75 1.75 0 0114.082 15H1.918a1.75 1.75 0 01-1.543-2.575L6.457 1.047zM9 11a1 1 0 11-2 0 1 1 0 012 0zm-.25-5.25a.75.75 0 00-1.5 0v2.5a.75.75 0 001.5 0v-2.5z", Svg.Attributes.fillRule "evenodd" ] [] ]

alert24Nodes : List (Svg msg)
alert24Nodes =
    [ Svg.path [ Svg.Attributes.d "M13 17.5a1 1 0 11-2 0 1 1 0 012 0zm-.25-8.25a.75.75 0 00-1.5 0v4.5a.75.75 0 001.5 0v-4.5z" ] [], Svg.path [ Svg.Attributes.d "M9.836 3.244c.963-1.665 3.365-1.665 4.328 0l8.967 15.504c.963 1.667-.24 3.752-2.165 3.752H3.034c-1.926 0-3.128-2.085-2.165-3.752L9.836 3.244zm3.03.751a1 1 0 00-1.732 0L2.168 19.499A1 1 0 003.034 21h17.932a1 1 0 00.866-1.5L12.866 3.994z", Svg.Attributes.fillRule "evenodd" ] [] ]
```

上記のような、SVG ファイルから `List (Svg msg)` 型の値に変換してそのまま書き出したのが `Nodes` モジュールである。
`Nodes` モジュールを参照して、使いやすいようにインターフェースを整えて、サイズ別に分けたのが `Small`・`Medium`・`Large` モジュールだ。
こんな感じ：

```elm
module Octicons.Medium exposing (...)

{-| `Html msg` values as SVG that size is 16px.

# SVG Icons

@docs accessibility, accessibilityInset, alert, ...
-}

import Html exposing (Html)
import Octicons.Internal as Octicons
import Octicons.Nodes as Octicons

...

{-| ref: <https://primer.style/octicons/alert-16>
-}
alert : List (Html.Attribute msg) -> Html msg
alert =
    Octicons.toSvg { name = "alert", size = 16 } Octicons.alert16Nodes
```

`Internal` モジュールは、サイズ別のモジュールで利用する関数を定義してある。
ドキュメント用のコメントアウトが無いと `elm publish` できないっぽいので、それも雑にだが生成するようにした。

## 更新検知

GitHub Actions の定期実行を使って、Octicons のリポジトリに新しいリリースがでたら通知する（メンション付きでPRを作る）ようにした：

```yaml
jobs:
  build:
    name: Build new version commit
    runs-on: ubuntu-latest
    env:
      VERSION_FILE: .octicons-version
      LATEST_LINK: https://api.github.com/repos/primer/octicons/releases/latest
    steps:
    - name: Checkout
      uses: actions/checkout@v2
      with:
        ref: main

    - name: Set current version
      id: current
      run: echo "version=$(cat $VERSION_FILE)" >> $GITHUB_OUTPUT

    - name: Set latest version
      id: latest
      run: echo "version=$(curl -s $LATEST_LINK | jq .tag_name | sed -e s/\"//g | sed -e s/^v//g)" >> $GITHUB_OUTPUT

    - name: Check exist branch # 同じバージョンのPRが重複しないように
      id: branch
      env:
        branch: octicons-version-${{ steps.latest.outputs.version }}
      run: echo "exist=$(git ls-remote --heads origin $branch | wc -l)" >> $GITHUB_OUTPUT

    - name: Update VERSION_FILE
      if: ${{ steps.latest.outputs.version != steps.current.outputs.version && steps.branch.outputs.exist == 0 }}
      env:
        NEW_VERSION: ${{ steps.latest.outputs.version }}
      run: echo "$NEW_VERSION" > $VERSION_FILE

    - name: Create Pull Request
      if: ${{ steps.latest.outputs.version != steps.current.outputs.version && steps.branch.outputs.exist == 0 }}
      uses: peter-evans/create-pull-request@v4
      with:
        token: ${{ secrets.GITHUB_TOKEN }}
        commit-message: 'Feat: update primer/octicons version'
        title: Release new primer/octicons version ${{ steps.latest.outputs.version }}
        body: |
         @matsubara0507
         - [x] update primer/octicons version file
         - [ ] update documents (README)
         - [ ] update sample workflow
        labels: New primer/octicons Version
        branch: octicons-version-${{ steps.latest.outputs.version }}
        base: main
        draft: true
```

自動生成に使った primer/octicons のバージョンを専用のファイル `.octicons-version` に保存し、最新のバージョンは GitHub Release の latest から取得して、それらを GitHub Actions の Outputs に保存して比較している。

## おしまい
