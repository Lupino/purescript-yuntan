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

import Prelude (Unit)
import Data.Argonaut.Core (Json)
import Yuntan.Trans (class DataSourceName, class DataSource, ServiceT, YuntanT, dataFetch, initService, Service)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Aff.Class (class MonadAff)
import Yuntan.Utils (importFn1, importFn2, importFn3)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Error.Class (class MonadThrow)

type Name = String
type NameSpace = String

type LQ = { from :: Int, size :: Int, type :: String }

defLQ :: LQ
defLQ = { from: 0, size: 10, type: "" }

type HQ = {from :: Int, size :: Int, start_time :: Int, end_time :: Int}

defHQ :: HQ
defHQ = {from: 0, size: 0, start_time: 0, end_time: 0}

initCoin :: forall opts eff. opts -> Eff eff Service
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
  | Graphql String
  | GraphqlByName Name String

instance dataSourceNameCoinReq :: DataSourceName CoinReq where
  dataSourceName _ = "coin"

instance dataSourceCoinReq :: (MonadAff eff m, MonadEff eff m) => DataSource m CoinReq where
  fetch = doFetch

doFetch
  :: forall eff m a. MonadAff eff m => MonadEff eff m
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
doFetch (Graphql ql) = importFn1 "graphql" ql
doFetch (GraphqlByName n ql) = importFn2 "graphqlByName" n ql

getScore
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => Name -> YuntanT m Json
getScore n = dataFetch (GetScore n)

getInfo
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => Name -> YuntanT m Json
getInfo n = dataFetch (GetInfo n)

putInfo
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => Name -> Json -> YuntanT m Unit
putInfo n v = dataFetch (PutInfo n v)

dropCoin
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => Name -> YuntanT m Unit
dropCoin n = dataFetch (DropCoin n)

getList
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => Name -> LQ -> YuntanT m Json
getList n lq = dataFetch (GetList n lq)

getListWithNameSpace
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => Name -> NameSpace -> LQ -> YuntanT m Json
getListWithNameSpace n ns lq = dataFetch (GetListWithNameSpace n ns lq)

getHistory
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => HQ -> YuntanT m Json
getHistory hq = dataFetch (GetHistory hq)

getHistoryByNameSpace
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => NameSpace -> HQ -> YuntanT m Json
getHistoryByNameSpace ns hq = dataFetch (GetHistoryByNameSpace ns hq)

save
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => Json -> YuntanT m Json
save v = dataFetch (Save v)

graphql
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => String -> YuntanT m Json
graphql ql = dataFetch (Graphql ql)

graphqlByName
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => Name -> String -> YuntanT m Json
graphqlByName n ql = dataFetch (GraphqlByName n ql)
