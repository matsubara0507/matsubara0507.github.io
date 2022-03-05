---
title: Haskell Servant + WebSocket で非同期ジョブを作る
tags: [Haskell]
---

とある理由で、Ruby の Resque や Sidekiq のような非同期ジョブを行うホビープログラムを Haskell で自作したくなったので、そのメモ書きです。
リポジトリはこちら：

[og:image style="max-width: 500px;"](https://github.com/matsubara0507/jobworker.hs)

## 非同期ジョブプログラム

雰囲気として、非同期ジョブの状態確認や実行を HTTP リクエストで受け取るサーバーを一つと、実際にジョブを処理するクライアントを複数用意します。
そして、サーバーとクライアントは WebSocket で繋がるイメージです。
ジョブの設定はサーバーが適当に YAML で読み込んで、クライアントへ渡します。
とりあえず、ジョブはただ単に `docker run` だけすることをゴールにします。

### Servant による Web API 

まずは、簡単な Web API をサーバー側に定義しておきます。
サーバーと接続したクライアントを返すだけです。
そのためにサーバーと接続したクライアントを表す型を定義しておきます。

```haskell
module JobWorker.Worker where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

newtype Id = Id Int32
　  deriving newtype (Show, Eq, Ord, Num, FromJSON, ToJSON)

data Worker = Worker
    { id :: Id
    , working :: Bool
    } deriving (Generic, Show, Eq)

instance ToJSON Worker
instance FromJSON Worker
```

GHC 9.2 のレコードドット記法を利用するので `id` というフィールド名をつけちゃいます（GHC 9.2 最高）。
`Worker` 型の値は雑に [STM](https://hackage.haskell.org/package/stm) を利用して状態管理することにします。

```haskell
module JobWorker.DB where

import Control.Concurrent.STM qualified as STM

data DB = DB
    { workers :: STM.TVar (Map Worker.Id (Maybe Worker))
    }

new :: IO DB
new = DB <$> STM.newTVarIO mempty

getAllWorker :: DB -> IO [Worker]
getAllWorker db = 
    catMaybes . Map.elems <$> STM.readTVarIO db.workers
```

`Maybe Worker` 型を利用しているのは、確保した ID が被らないようにクライアントのコネクションが切れたら `Nothing` で置き換えるためです。
WebSocket のコネクションを取得して、`DB` 型に `Worker` 型の値を保存する部分は後述します。

そして、Web API の定義には [Servant](https://docs.servant.dev/) を使います。

```haskell
module JobWorker.Server where

import JobWorker.DB qualified as DB
import Servant

type API = "api" :> WebAPI

api :: Proxy API
api = Proxy

type WebAPI
    = "workers" :> Get '[JSON] [Worker]

server :: DB -> Server API
server db 
    = liftIO (DB.getAllWorker db)
```

あとは適当に `main` を書いて起動し、`curl` を使ってリクエストを投げてみます。

```
$ curl -s localhost:8080/api/workers | jq
[]
```

### Server-Client を WebSocket で繋ぐ

Haskell で WebSocket を利用する簡単な方法は [websockets パッケージ](https://hackage.haskell.org/package/websockets-0.12.7.3)を使うことです。
まずは `Worker` 型を拡張してコネクションを持たせます。

```haskell
module JobWorker.Worker where

import Network.WebSockets qualified as WS

data Worker = Worker
    { id :: Id
    , conn :: WS.Connection
    , working :: Bool
    }
 
new :: Id -> WS.Connection -> Worker
new wid conn = Worker wid conn False

data Info = Info
    { id :: Id
    , working :: Bool
    } deriving (Generic, Show, Eq)
```

`WS.Connection` 型は、さすがに JSON へエンコードできないので、Web API で返す用の型として `Worker.Info` 型を用意しました。
次に WebSocket サーバー側を定義します。
WebSocket のルーティングを Servant に乗せる簡単な方法は [servant-websockets パッケージ](https://hackage.haskell.org/package/servant-websockets)を使うことです：

```haskell
module JobWorker.Server where

import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Control.Monad (forever)
import Network.WebSockets qualified as WS

type API
    = "api" :> JobAPI
 :<|> "runner" :> WS.WebSocket

server :: DB -> Server API
server db
    = liftIO (DB.getAllWorker db)
 :<|> (liftIO . serveRunner config db)

serveWorker :: DB -> WS.Connection -> IO ()
serveWorker db conn = do
    worker <- DB.connectedWorker db conn
    forever (threadDelay 1_000_000) `finally` DB.disconnectedWorker db worker.id
```

まだメッセージのやりとりはせず、ただ `db` にコネクションを保存して、`threadDelay` でひたすら待ってるだけです。
`connectedWorker` が STM に `Worker` 型の値を保存する関数で、`disconnectedWorker` が接続が切れたとして `Nothing` を保存する関数です：

```haskell
module JobWorker.DB where

connectedWorker :: DB -> WS.Connection -> IO Worker
connectedWorker db conn = STM.atomically $ do
    maxId <- Map.size <$> STM.readTVar db.workers
    let worker = Worker.new (fromIntegral $ maxId + 1) conn
    STM.modifyTVar db.workers (Map.insert worker.id $ Just worker)
    pure worker

disconnectedWorker :: DB -> Worker.Id -> IO ()
disconnectedWorker db wid =
    STM.atomically $ STM.modifyTVar db.workers (Map.update (\_ -> Just Nothing) wid)
```

あとはクライアント側を定義します。
websockets パッケージにはクライアント側の関数や型もあります：

```haskell
module JobWorker.Client where

import Network.WebSockets qualified as WS

data Client = Client
  { host :: String
  , port :: Int
  , path :: String
  }

-- 'localhost:8080/hoge/fuga' を適当にパースする
new :: String -> Maybe Client
new dest = ...

run :: Client -> IO ()
run client = WS.runClient client.host client.port client.path $ \conn ->
    WS.withPingThread conn 15 (pure ()) $ do
        forever (threadDelay 1_000_000)
```

`withPingThread` によって第一引数で与えたコネクションに対し、第二引数で与えた秒毎で Ping を別スレッドで送り続けます。
WebSocket サーバーは一定時間やり取りがない場合にタイムアウトする可能性があり、多くの場合はタイムアウトが60秒なので半分の30秒にして動作させておくと良いと Hackage には書いてあります。
Haskell の Warp の場合（`Warp.run`）は[デフォルトだと30秒でタイムアウトするようになっている](https://hackage.haskell.org/package/warp-3.3.19/docs/src/Network.Wai.Handler.Warp.Settings.html#defaultSettings)ので、半分の15秒にしてみました（サーバー側をいじっても良い）。

これらを適当に Docker イメージ化して、docker-compose で起動した後に curl すると無事クライアントが接続されました：

```
$ curl -s localhost:8080/api/workers | jq
[
  {
    "id": 1,
    "working": false
  },
  {
    "id": 2,
    "working": false
  },
  {
    "id": 3,
    "working": false
  }
]
```

### WebSocket上のプロトコルを定義

サーバー・クライアント間でやり取りするためのプロトコルっぽいものを定義します。
websockets パッケージの場合は `WebSocketsData` 型クラスというのがあります。
これを使うことで WebSockets 上でやりとりする `ByteString` 型と任意の型への相互変換を定義できます。
例えば、サーバーからクライアントへ送るデータ型を次のように定義しました：

```haskell
module JobWorker.Protocol where

import Data.Aeson qualified as JSON
import Data.Binary qualified as Binary
import Data.ByteString.Lazy qualified as LBS
import JobWorker.Job qualified as Job
import Network.WebSockets qualified as WS

data Server
  = JobConfigs [Job.Config]
  | Enqueue Job.Id Job.Name
  | SUndefined

instance WS.WebSocketsData Server where
  fromDataMessage (WS.Text   bl _) = WS.fromLazyByteString bl
  fromDataMessage (WS.Binary bl)   = WS.fromLazyByteString bl

  fromLazyByteString lbs = case LBS.uncons lbs of
    Just (1, rest) ->
      case JSON.decode rest of
        Just configs ->
          JobConfigs configs
        Nothing ->
          SUndefined lbs

    Just (2, rest) ->
      uncurry Enqueue (Binary.decode rest)

    _ ->
      SUndefined

  toLazyByteString p = case p of
    JobConfigs configs ->
      LBS.cons 1 (JSON.encode configs)

    Enqueue wid name ->
      LBS.cons 2 (Binary.encode (wid, name))

    SUndefined ->
      ""
```

クライアントへ送るメッセージの種類は、`ByteString` の先頭で判定することにしました。
`Job.Config` 型は YAML で定義したジョブの設定を読み込んだ型です。
[aeson パッケージ](https://hackage.haskell.org/package/aeson)で JSON にして送ります。
ジョブ（`Job` 型）はサーバー側で生成して、クライアントへ処理を任せます。
`Job.Id` 型は、その生成したジョブのユニークIDで、`Job.Name` 型は `Job.Config` にあるジョブの種類を指すユニークIDです。
[`Binary` 型クラス](https://hackage.haskell.org/package/binary-0.10.0.0/docs/Data-Binary.html#t:Binary)を利用してエンコード・デコードをすることにしました。

クライアントからサーバーへはジョブの状態を返すようなプロトコルを定義しました：

```haskell
data Client
  = JobRunning Job.Id
  | JobSuccess Job.Id
  | JobFailure Job.Id
  | CUndefined

instance WS.WebSocketsData Client where
  fromDataMessage (WS.Text   bl _) = WS.fromLazyByteString bl
  fromDataMessage (WS.Binary bl)   = WS.fromLazyByteString bl

  fromLazyByteString lbs = case LBS.uncons lbs of
    Just (1, rest) ->
      JobRunning (Binary.decode rest)

    Just (2, rest) ->
      JobSuccess (Binary.decode rest)

    Just (3, rest) ->
      JobFailure (Binary.decode rest)

    _ ->
      CUndefined

  toLazyByteString p = case p of
    JobRunning wid ->
      LBS.cons 1 (Binary.encode wid)

    JobSuccess wid ->
      LBS.cons 2 (Binary.encode wid)

    JobFailure wid ->
      LBS.cons 3 (Binary.encode wid)

    CUndefined ->
      ""
```

websockets パッケージでメッセージを送受信するには `sendBinaryData` 関数と `receiveData` 関数を使います：

```haskell
-- Server 側
serveWorker :: [Job.Config] -> DB -> WS.Connection -> IO ()
serveWorker configs db conn = do
  worker <- DB.connectedWorker db conn
  WS.sendBinaryData worker.conn (Protocol.JobConfigs configs)
  forever (receive worker) `finally` DB.disconnectedWorker db worker.id
  where
    receive worker = do
      p <- WS.receiveData worker.conn
      case p of
        Protocol.JobRunning jid ->
          DB.runningJob db worker.id jid
        Protocol.JobSuccess jid ->
          DB.successJob db worker.id jid
        Protocol.JobFailure jid ->
          DB.failureJob db worker.id jid
        _ ->
          pure ()

-- Client 側
run :: Client -> IO ()
run client = WS.runClient client.host client.port client.path $ \conn ->
  WS.withPingThread conn 15 (pure ()) $ do
    forever (receive conn)
  where
    receive conn = do
      p <- WS.receiveData conn
      case p of
        Protocol.JobConfigs configs ->
          STM.atomically $ STM.writeTVar client.configs configs
        Protocol.Enqueue jid jname -> 
          runJob conn client jid jname
        _ ->
          pure ()

runJob :: WS.Connection -> Client -> Job.Id -> Job.Name -> IO ()
runJob conn client jid jname = do
  configs <- STM.atomically $ STM.readTVar client.configs
  case List.find (\config -> config.name == jname) configs of
    Nothing ->
      WS.sendBinaryData conn (Protocol.JobFailure jid)
    Just config -> do
      WS.sendBinaryData conn (Protocol.JobRunning jid)
      threadDelay 10_000_000
      WS.sendBinaryData conn (Protocol.JobSuccess jid)
```

クライアントは、サーバーから受け取った設定の情報を STM で保存することにします。
現状はとりあえず、10秒待って成功のメッセージを返すようにします。
これだけだとまだ、ジョブを積む側の処理がありません。ジョブは Web API を介して積むようにしました：

```haskell
type JobAPI
    = "workers" :> Get '[JSON] [Worker.Info]
 :<|> "jobs" :> Get '[JSON] [Job]
 :<|> "jobs" :> Capture "name" Job.Name :> Post '[JSON] Job

server :: [Job.Config] -> DB -> Server API
server configs db
      = (getWorkers :<|> getJobs :<|> kickJob)
   :<|> (liftIO . serveRunner config db)
  where
    getWorkers = liftIO $ DB.getAllWorkerInfo db
    getJobs = liftIO $ DB.getAllJob db
    kickJob name =
      case List.find (\config -> config.name == name) configs of
        Nothing ->
          throwError err404
        Just _ -> do
          w <- liftIO $ randomWorker db
          case w of
            Nothing ->
              throwError $ err500 { errBody = "worker is not exist." }
            Just worker -> liftIO $ do
              job <- DB.enqueueJob db name
              WS.sendBinaryData worker.conn (Protocol.Enqueue job.id job.name)
              pure job
```

`Job` 型も `Worker` 型と同じように STM で管理します（割愛）。
クライアントは複数あるので、ランダムで1つ取得するための処理が `randomWorker` 関数です。
雑に `randomRIO` 関数を使ってとってくるだけです：

```haskell
import System.Random (randomRIO)

randomWorker :: DB -> IO (Maybe Worker)
randomWorker db = do
  workers <- DB.getAllWorker db
  case workers of
    [] ->
      pure Nothing
    _ -> do
      idx <- (\x -> x - 1) <$> randomRIO (1, length workers)
      pure $ Just (workers !! idx)
```

Docker イメージを更新して、適当に curl するとこうなります：

```
$ curl -s localhost:8080/api/workers | jq
[
  {
    "id": 1,
    "working": false
  },
  {
    "id": 2,
    "working": false
  }
]
$ curl -s localhost:8080/api/jobs | jq
[]
$ curl -s -XPOST localhost:8080/api/jobs/hello-world | jq
{
  "id": 1,
  "name": "hello-world",
  "queuing": true,
  "running": false,
  "success": false
}
$ curl -s localhost:8080/api/workers | jq
[
  {
    "id": 1,
    "working": false
  },
  {
    "id": 2,
    "working": true
  }
]
$ curl -s localhost:8080/api/jobs | jq
[
  {
    "id": 1,
    "name": "hello-world",
    "queuing": false,
    "running": true,
    "success": false
  }
]
```

### Client にキューを導入

`runJob` のところでジョブを実行しても良いですが、そうすると WebSocket が詰まってしまいそうなので、クライアント側に非同期用のキューをサクッと実装します。
Haskell では [stm パッケージの `TQueue` 型](https://hackage.haskell.org/package/stm-2.5.0.2/docs/Control-Concurrent-STM-TQueue.html)を利用することで、簡単に非同期処理用のメッセージキューを実装できます：

```haskell
module JobWorker.Client where
...

data Client = Client
  { host    :: String
  , port    :: Int
  , path    :: String
  , configs :: STM.TVar [Job.Config]
  , queue   :: STM.TQueue Job -- コレ
  }

run :: Client -> IO ()
run client = WS.runClient client.host client.port client.path $ \conn ->
  WS.withPingThread conn 15 (pure ()) $ do
    forkIO $ forever (runJob conn client) -- ジョブの処理を非同期にする
    forever (receive conn)
  where
    receive conn = do
      p <- WS.receiveData conn
      case p of
        Protocol.Enqueue jid jname ->
          -- WebSocketからのをそのままキューに積むだけ
          STM.atomically $ STM.writeTQueue client.queue (Job.new jname jid)
        ...

runJob :: WS.Connection -> Client -> IO ()
runJob conn client = do
  -- キューからジョブを取り出す（取り出せるまで待つ）
  job <- STM.atomically $ STM.readTQueue client.queue
  configs <- STM.atomically $ STM.readTVar client.configs
  ...
```

### Docker コンテナの実行

最後に、ジョブを Docker コンテナで実行します。
といっても、ただプロセスを雑に実行するだけです。
外部プロセスを実行するのには [process パッケージの `eadProcessWithExitCode` 関数](https://hackage.haskell.org/package/process-1.6.14.0/docs/System-Process.html#v:readProcessWithExitCode) を使います：

```haskell
module JobWorker.Docker where

import JobWorker.Job qualified as Job
import System.Exit (ExitCode)
import System.Process qualified as Process

run :: Job.Config -> IO (ExitCode, String, String)
run config =
  Process.readProcessWithExitCode "docker" ["run", "--rm", config.image, config.command] ""
```

最後の引数 `""` はプロセスに与える標準入力なので空文字列です。
後はこれをクライアントで呼び出して、結果に応じてサーバーへの返答を変えるだけです。

## おしまい