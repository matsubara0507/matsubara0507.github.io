---
title: Haskell による JSON Parser
---

タイトルの通り．
Haskell の Functional Parser ライブラリ，Parsec を使って，JSON Parser を作った話．
ありきたりですね．

作ったコードのリポジトリは[ココ](https://github.com/matsubara0507/jsonparser)です．

## いきさつ

名〇屋でやってる(数少ない FP な)勉強会，「[FP in Scala 読書会](https://fp-in-scala-nagoya.connpass.com/)」に6月ぐらいから参加している．
前々回ぐらいから第8章の「関数型パーサー」に入って，猛烈な盛り上がりを見せている(？)．

書籍中の例題として JSON Parser を書くのだが，目の前の人が無限に「[json.org](http://json.org) は素晴らしい」と言っており，洗脳されたため，サクッと JSON Parser を書きたくなってしまった．

まぁサクッと書くなら Haskell ですよねってことで書いてみた(Scala で書けよ)．

## 作る

### 型

(関数型プログラミングの常套手段として)まずは型を用意する．

```Haskell
type JSON = [(String, JValue)]  -- (1)
data JValue = JNull             
            | JNumber Double
            | JString String
            | JBool   Bool
            | JArray  [JValue]
            | JObject JSON      -- (2)
            deriving (Show, Eq)
```

(1)のように，わざわざ型シノニムする必要は全くない((2)の `JSON` を置き換えればよい)が，頭の整理がつきやすかったので敢えて書いた．
言語によっては連想配列を使っていると思うが，どーせ頭から順にパースした結果を突っ込むだけなので，解析中はリストで十分なはず．

数値に `Double` 型を使ってるのも気になるが，[FP in Scala](http://book.impress.co.jp/books/1114101091)でも `Double` 使ってるからこれでいいかな．

```Scala
trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON
}
```

Scala と比べるとすごいシンプルに書けるよね．
すばらしい．

### パーサー

後は，JSON の BNF みて実装するだけ．

ちなみに，関数型プログラミングなのでトップダウン的に考える．
細部はまだ `undefined` にして，定期的にコンパイルすべき．
例えば

```Haskell
jsonParser :: Parser JSON
jsonParser = objectParser

objectParser :: Parser JSON
objectParser = between (string "{") (string "}") membersParser

membersParser :: Parser JSON
membersParser = undefined
```

と書いて，コンパイルし，問題なければ


```Haskell
membersParser :: Parser JSON
membersParser = pairParser `sepBy` char ','

pairParser :: Parser (String, JValue)
pairParser = undefined
```

と次の階層へ降りていく．

ちなみに `string`, `between`, `sepBy`, `char` は [megaparsec](https://hackage.haskell.org/package/megaparsec) というライブラリに標準である関数(もとい Parsec 系のライブラリにはだいたいある)．
本当に便利．

### 工夫

殆んどない．
強いてあげるなら，Monad は使わずに Applicative だけで書いた．
JSON は文脈自由文法なので bind (flatMap, do記法) は要らない(と FP in Scala には書いてあった)．

その代わり，読みにくい関数もあるけどね．

これとか

```Haskell
pairParser :: Parser (String, JValue)
pairParser = (,) <$> (token stringParser <* char ':') <*> token valueParser
```

あとは，様々な記法の数値を変換するのだるくて `read` 使った(そのために文字列を返して最後に読んでる)．

### 今後

GADT と存在型(?)を使って，`Double` を任意の `Num` 型クラスのインスタンス型を解釈させてもいいかも．

```Haskell
data JValue where
  JNull   :: JValue
  JNumber :: Num a => a -> JValue
  JString :: String -> JValue
  JBool   :: Bool -> JValue
  JArray  :: [JValue] -> JValue
  JObject :: JSON -> JValue
```

うまくいくかはわかんない．
`read` 使ってるからつらそう．

## おしまい
