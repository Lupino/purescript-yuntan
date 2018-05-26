module Yuntan.Coin
  ( Name
  , NameSpace
  , LQ (..)
  , defLQ
  , HQ (..)
  , defHQ

  , initCoin

  , getScore
  , getInfo
  , putInfo
  , dropCoin
  , getList
  , getListWithNameSpace
  , getHistory
  , getHistoryByNameSpace
  , save
  , graphql
  , graphqlByName
  ) where

import Prelude (Unit, (<<<))
import Data.Argonaut.Core (Json)
import Yuntan.Trans (class DataSourceName, class DataSource, ServiceT, YuntanT, dataFetch, initService, Service)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Yuntan.Utils (importFn1, importFn2, importFn3)
import Effect.Exception (Error)
import Control.Monad.Error.Class (class MonadThrow)

type Name = String
type NameSpace = String

type LQ = { from :: Int, size :: Int, type :: String }

defLQ :: LQ
defLQ = { from: 0, size: 10, type: "" }

type HQ = {from :: Int, size :: Int, start_time :: Int, end_time :: Int}

defHQ :: HQ
defHQ = {from: 0, size: 0, start_time: 0, end_time: 0}

initCoin :: forall opts. opts -> Effect Service
initCoin = initService "coin" "CoinService"

data CoinReq =
    GetScore Name
  | GetInfo Name
  | PutInfo Name Json
  | DropCoin Name
  | GetList Name LQ
  | GetListWithNameSpace Name NameSpace LQ
  | GetHistory HQ
  | GetHistoryByNameSpace NameSpace HQ
  | Save Json
  | GraphQL String
  | GraphQLByName Name String

instance dataSourceNameCoinReq :: DataSourceName CoinReq where
  dataSourceName _ = "coin"

instance dataSourceCoinReq :: MonadAff m => DataSource m CoinReq where
  fetch = doFetch

doFetch
  :: forall m a. MonadAff m
  => CoinReq -> ServiceT m a

doFetch (GetScore n) = importFn1 "getScore" n
doFetch (GetInfo n) = importFn1 "getInfo" n
doFetch (PutInfo n v) = importFn2 "putInfo" n v
doFetch (DropCoin n) = importFn1 "dropCoin" n
doFetch (GetList n lq) = importFn2 "getList" n lq
doFetch (GetListWithNameSpace n ns lq) = importFn3 "getListWithNameSpace" n ns lq
doFetch (GetHistory hq) = importFn1 "getHistory" hq
doFetch (GetHistoryByNameSpace ns hq) = importFn2 "getHistoryByNameSpace" ns hq
doFetch (Save v) = importFn1 "save" v
doFetch (GraphQL ql) = importFn1 "graphql" ql
doFetch (GraphQLByName n ql) = importFn2 "graphqlByName" n ql

getScore
  :: forall m. MonadAff m => MonadThrow Error m
  => Name -> YuntanT m Int
getScore = dataFetch <<< GetScore

getInfo
  :: forall m. MonadAff m => MonadThrow Error m
  => Name -> YuntanT m Json
getInfo = dataFetch <<< GetInfo

putInfo
  :: forall m. MonadAff m => MonadThrow Error m
  => Name -> Json -> YuntanT m Unit
putInfo n = dataFetch <<< PutInfo n

dropCoin
  :: forall m. MonadAff m => MonadThrow Error m
  => Name -> YuntanT m Unit
dropCoin = dataFetch <<< DropCoin

getList
  :: forall m. MonadAff m => MonadThrow Error m
  => Name -> LQ -> YuntanT m Json
getList n = dataFetch <<< GetList n

getListWithNameSpace
  :: forall m. MonadAff m => MonadThrow Error m
  => Name -> NameSpace -> LQ -> YuntanT m Json
getListWithNameSpace n ns = dataFetch <<< GetListWithNameSpace n ns

getHistory
  :: forall m. MonadAff m => MonadThrow Error m
  => HQ -> YuntanT m Json
getHistory = dataFetch <<< GetHistory

getHistoryByNameSpace
  :: forall m. MonadAff m => MonadThrow Error m
  => NameSpace -> HQ -> YuntanT m Json
getHistoryByNameSpace ns = dataFetch <<< GetHistoryByNameSpace ns

save
  :: forall m. MonadAff m => MonadThrow Error m
  => Json -> YuntanT m Json
save = dataFetch <<< Save

graphql
  :: forall m. MonadAff m => MonadThrow Error m
  => String -> YuntanT m Json
graphql = dataFetch <<< GraphQL

graphqlByName
  :: forall m. MonadAff m => MonadThrow Error m
  => Name -> String -> YuntanT m Json
graphqlByName n = dataFetch <<< GraphQLByName n
