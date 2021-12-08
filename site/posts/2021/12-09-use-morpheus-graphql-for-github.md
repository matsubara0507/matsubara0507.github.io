---
title: Haskell Morpheus GraphQL で GitHub API を試す
tags: [Haskell, GraphQL, GitHub]
---

本記事は 「[Haskell Advent Calendar 2021](https://qiita.com/advent-calendar/2021/haskell)」の9日目の記事です。

2021年11月に開催された「[Haskell Day 2021](https://haskell.jp/haskell-day-2021/)」の発表で [Morpheus GraphQL](https://morpheusgraphql.com/) の紹介がありました。
それに触発されて Morpheus GraphQL を触ってみます。
Haskell Day での発表では、GraphQL サーバーの構築について焦点が当てられてましたが、私はクライアント側を試してみます。

## 題材: stack-tpls

題材として私が自作している [stack-tpls](https://github.com/matsubara0507/stack-tpls) というツールを使います。
これは、Haskell Stack の公開されているテンプレートの一覧表示などをしてくれる CLI ツールです：

```
$ stack-tpls --list | grep matsubara0507
github:matsubara0507/get-opt-cli.hsfiles
github:matsubara0507/lib-extensible.hsfiles
github:matsubara0507/mix-cli-with-bazel.hsfiles
github:matsubara0507/mix-cli.hsfiles
github:matsubara0507/optparse-applicative-cli.hsfiles
```

GitHub や GitLab などの stack-templates というリポジトリのトップレベルにある `.hsfiles` ファイルがテンプレートになります。
GitHub から Stack のテンプレートを取得するのに [GitHub GraphQL API](https://docs.github.com/graphql) を使っています。
`stack-templates` が名前に含まれるリポジトリを検索し、クエリの結果にはトップレベルの Git オブジェクトを含ませて取得します。
あとは、クエリの結果のうち条件に当てはまるものをフィルタリングしています。
で、元々は手書きでクエリを組み立てたのですが：

```hs
searchQuery :: Text -> Maybe Text -> Text
searchQuery query after = mconcat
  [ "query{search("
  , "query:", tshow query, ", "
  , "type: REPOSITORY, ",
  , "first: 100",
  , maybe "" (\txt -> ", after: " <> tshow txt) after
  , "){", Text.intercalate "," fields, "}}"
  ]
  where
    fields =
      [ "repositoryCount"
      , "pageInfo{ endCursor, hasNextPage }"
      , "edges{ node{ ... on Repository{ nameWithOwner, name, " <> obj <> " }}}"
      ]
    obj = "object(expression:\"HEAD\"){ ... on Commit{ tree{ entries{ name, type }}}}"
```

そこを Morpheus GraphQL に置き換えます。

## Morpheus GraphQL でクライアント作成

Morpheus GraphQL でクライアントだけを作る場合は [morpheus-graphql-client パッケージ](https://hackage.haskell.org/package/morpheus-graphql-client-0.18.0)を使います。
GraphQL のスキーマファイルを用意し、`defineByDocumentFile` を使って Template Haskell で GraphQL クエリに対応する Haskell の関数を生成します。
例えば、前述した stack-tpls のクエリを記述すると次のようになります：

```hs
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import           Data.Morpheus.Client

defineByDocumentFile "./assets/schema.docs.graphql"
  [gql|
    query SearchRepository($query: String!, $cursor: String) {
      search(query: $query, type: REPOSITORY, first: 100, after: $cursor) {
        repositoryCount,
        pageInfo { endCursor, hasNextPage }
        edges {
          node {
            ... on Repository {
              __typename
              nameWithOwner,
              name,
              object(expression: "HEAD") {
                ... on Commit { __typename, tree { entries { name, type } } }
              }
            }
          }
        }
      }
    }
  |]
```

スキーマは、[GitHub の場合はココに公開されています](https://docs.github.com/graphql/overview/public-schema)。
公開されていなくても、API から生成する方法があるらしいです（詳しくは知らない）。

で、この Template Haskell は例えば次のようなクエリの結果を表現した型を生成します：

```hs
data SearchRepository = SearchRepository { search :: SearchSearchResultItemConnection	}

data SearchSearchResultItemConnection = SearchSearchResultItemConnection
  { repositoryCount :: Int	 
  , pageInfo :: SearchPageInfoPageInfo	 
  , edges :: Maybe [Maybe SearchEdgesSearchResultItemEdge]
  }

data SearchPageInfoPageInfo = SearchPageInfoPageInfo	 
  { endCursor :: Maybe Text	 
  , hasNextPage :: Bool
  }

data SearchEdgesSearchResultItemEdge = SearchEdgesSearchResultItemEdge
  { node :: Maybe SearchEdgesNodeSearchResultItem
  }

data SearchEdgesNodeSearchResultItem
  = SearchEdgesNodeSearchResultItem { __typename :: Text }
  | SearchEdgesNodeRepository -- こっちしか来ないけど
      { __typename :: Text	 
      , nameWithOwner :: Text	 
      , name :: Text	 
      , object :: Maybe SearchEdgesNodeObjectGitObject
      }

data SearchEdgesNodeObjectGitObject
  = SearchEdgesNodeObjectGitObject { __typename :: Text }
  | SearchEdgesNodeObjectCommit -- こっちしか来ないけど
      { __typename :: Text
      , tree :: SearchEdgesNodeObjectTreeTree
      }

data SearchEdgesNodeObjectTreeTree = SearchEdgesNodeObjectTreeTree
  { entries :: Maybe [SearchEdgesNodeObjectTreeEntriesTreeEntry]
  }

data SearchEdgesNodeObjectTreeEntriesTreeEntry = SearchEdgesNodeObjectTreeEntriesTreeEntry	 
  { name :: Text	 
  , type' :: Text -- type は type' になる
  }
```

GraphQL はクエリで返して欲しいデータ構造（型構造）自体を構築するため、このようなデータ型をクエリ毎に Template Haskell で生成するわけですね。
クエリ結果の型には morpheus-graphql-client パッケージの `Fetch` 型クラスのインスタンスが定義されています。
この型クラスを利用してクエリを実行するのです：

```hs
class Fetch a where
  type Args a :: *
  fetch 
    :: (Monad m, FromJSON a) 
    => (ByteString -> m ByteString)
    -> Args a 
    -> m (Either (FetchError a) a)

instance Fetch SearchRepository where
  type Args SearchRepository = SearchRepositoryArgs
  fetch = ...

data SearchRepositoryArgs = SearchRepositoryArgs
  { query :: Text
  , cursor :: Maybe Text
  }
```

`SearchRepositoryArgs` 型も Template Haskell で生成されます。
`fetch` 関数の一引数目には、実際に `ByteString` のクエリ文字列を受け取り HTTP リクエストをして `ByteString` の結果を返す関数を渡します。
[morpheus-graphql-client パッケージの README](https://github.com/morpheusgraphql/morpheus-graphql/blob/1ad3b8d59a1e4f62628dfdcae204c768d2b5b0f7/morpheus-graphql-client/README.md)には [req パッケージ](https://hackage.haskell.org/package/req-3.9.2)を使った例があるので、参考にして次のように書きました：

```hs
import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Lazy as Lazy
import           Network.HTTP.Req

resolver :: ByteString -> Lazy.ByteString -> IO Lazy.ByteString
resolver token b = runReq defaultHttpConfig $ do
    let headers = header "Content-Type" "application/json"
               <> header "User-Agent" "matsubara0507/stack-tpls"
               <> oAuth2Bearer token
    responseBody <$> req POST (https "api.github.com" /: "graphql") (ReqBodyLbs b) lbsResponse headers
```

余談ですが、Template Haskell の生成結果は `stack haddock` などを利用して Haddock を生成して利用すると確認しやすいです（そのときに `--no-haddock-deps` をすれば、依存パッケージの Haddock 生成をスキップできるので早いです）。

### 注意点

いくつか注意点があります。

まず、GitHub GraphQL API で morpheus-graphql-client パッケージを使うには v0.18.0 以上を使う必要があります。
もし古いのを使うと、次のようなエラーメッセージが出ます：

```
stack-tpls/src/StackTemplates/GitHub/GraphQL/Query.hs:14:1: error:
    [{"message":"offset=672270:\nunexpected 'Â'\nexpecting \"\"\"\"\", newline, or printable character\n","locations":[{"line":36050,"column":60}]}]
   |
14 | defineByDocumentFile "./assets/schema.docs.graphql"
   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^...
```

これは、[この Issue のやつ](https://github.com/morpheusgraphql/morpheus-graphql/issues/581)でスキーマのパーサーが全ての Unicode な文字を扱えてなかったのが原因です。
GitHub GraphQL API のスキーマの36050行目のコメントに、そういう文字が含まれていたというわけです。
幸いにも、すでに修正されているので、バージョンを上げておきましょう。

#

もう一つは `__typename` の部分です。
これをなくすとコンパイルはできますが、実行時に次のようなエラーメッセージが出てしまいます：

```
$ stack-tpls --list --update
FetchErrorParseFailure "Error in $.data.search.edges[0].node: key \"__typename\" not found on object"
```

GraphQL には [Interface Type](https://graphql.org/learn/schema/#interfaces) というのがあります。
特定のフィールドを持っている型を多相的に扱えるというものです。
クエリでは、この Interface Type に対して「具体的なこの型が欲しい」と指定できます。
`... on Repository` や `... on Commit` の部分です。

で、クエリ結果の JSON には [`__typename` というフィールド](https://graphql.org/learn/queries/#meta-fields)を使うことで、どの具体型なのかの情報を載せることができます。
おそらく、Morpheus GraphQL のパーサーは `__typename` を使って変換先の型を選んでるんだと思います。
そのため、勝手に生成する型構造には `__typename` フィールドを付与されるのですが、リクエストしてるクエリには `__typename` が含まれないため、実際の結果の JSON には含まれてなくて落ちてるわけですね。
なので、`... on` する場合はとりあえず `__typename` 付けることで回避できます。

## おまけ：別々のクエリ結果を多相的に扱う

Morpheus GraphQL のクライアントでは、クエリに対して別々の型を生成します。
例えば、`SearchRepository` クエリの他に、次のようなクエリを生成してみます：

```hs
defineByDocumentFile "./assets/schema.docs.graphql"
  [gql|
    query GetReository($owner: String!, $name: String!) {
      repository(owner: $owner, name: $name) {
        object(expression: "HEAD") {
          ... on Commit { __typename, tree { entries { name, type } } }
        }
      }
    }
  |]
```

指定したリポジトリのトップレベルの Git オブジェクトをとってきているだけですね。
これは次のような型を生成します：

```hs
data GetReository = GetReository { repository :: Maybe RepositoryRepository } 

data RepositoryRepository = RepositoryRepository { object :: Maybe RepositoryObjectGitObject }

data RepositoryObjectGitObject
  = RepositoryObjectGitObject { __typename :: Text }
  | RepositoryObjectCommit 
      { __typename :: Text
      , tree :: RepositoryObjectTreeTree
      }

data RepositoryObjectTreeTree = RepositoryObjectTreeTree 
  { entries :: Maybe [RepositoryObjectTreeEntriesTreeEntry]
  }

data RepositoryObjectTreeEntriesTreeEntry = RepositoryObjectTreeEntriesTreeEntry
  { name :: Text	 
  , type' :: Text
  }
```

生成した型のうち、`SearchRepository` クエリから生成したものとフィールドもとい本質的に同じだけど型名が違うものがいくつかありますね。
例えば、`SearchEdgesNodeObjectTreeEntriesTreeEntry` と `RepositoryObjectTreeEntriesTreeEntry` です。
できれば、この両方に対して同じ関数を定義したいなと思いませんか？

いくつかやり方はあると思いますが、今回は GHC 9.2.1 で導入された `OverloadedRecordDot` 言語拡張を利用します。

### `OverloadedRecordDot` 言語拡張

この言語拡張自身については、だめぽ氏の「[GHC 9.2の新機能と、GHCの動向2021](https://zenn.dev/mod_poppo/articles/ghc-9-2-and-future#record-dot-syntax)」を参考にしてください（ないしは適当に調べてください）。
で、`OverloadedRecordDot` を実現するために [`GHC.Records` モジュールと `HasField` 型クラス](https://hackage.haskell.org/package/base-4.16.0.0/docs/GHC-Records.html#t:HasField)が追加されましたね？

```hs
class HasField x r a | x r -> a where
  getField :: r -> a
```

この型クラスは「型 `r` のレコードが `a` 型を返すフィールド `x` を持つ」というのを表しています。
つまり、これを使えば「同一のフィールドを持つレコード型」に対して共通の関数を定義できるわけです：

```hs
-- name の方は厳密にはいらないが
isBlob :: (HasField "name" r Text, HasField "type'" r Text) => r -> Bool
isBlob entry = entry.type' == "blob"
```

`isBlob` 関数は `SearchEdgesNodeObjectTreeEntriesTreeEntry` 型と `RepositoryObjectTreeEntriesTreeEntry` 型の両方に適用できる関数です。
これを駆使すれば、別々のクエリ結果に対する関数をいい感じに共通化できそうですね。
ただし、GraphQL の Interface Type を挟むと直和型を含むのでうまく表現できなくなります。

## おしまい

ちなみに、おまけ部分のコードは [matsubara0507/github-graphql-example.hs の ghc9.2 ブランチ](https://github.com/matsubara0507/github-graphql-example.hs/tree/ghc9.2)に置いてあります。
Morpheus GraphQL のパッケージを GHC 9.2.1 で使うために、いろいろフォークして修正しています（現在は、ですが）。
