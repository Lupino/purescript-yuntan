module Yuntan.Share
  ( Q (..)
  , defQ
  , initShare
  , create
  , createHistory
  , saveConfig
  , getConfig
  , getList
  , statistic
  , get
  , getChildren
  , getHistory
  , getPatch
  , graphql
  ) where

import Prelude (Unit, (<<<))
import Data.Argonaut.Core (Json)
import Yuntan.Internal.Trans (class DataSourceName, class DataSource, ServiceT, YuntanT, dataFetch, Service)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Yuntan.Internal.Utils (fromFn1, fromFn2)
import Effect.Exception (Error)
import Control.Monad.Error.Class (class MonadThrow)
import Yuntan.Helper (initService)

type Name = String
type ShareName = String
type Key = String
type Value = String


type Q = { from :: Int, size :: Int }

defQ :: Q
defQ = { from: 0, size: 10 }

initShare :: forall opts. opts -> Effect Service
initShare = initService "share" "ShareService"

data ShareReq =
    Create ShareName Name
  | CreateHistory Name Json
  | SaveConfig Key Value
  | GetConfig Key
  | GetList Q
  | Statistic Json
  | Get Name
  | GetChildren Name Q
  | GetHistory Name Q
  | GetPatch Name Json
  | GraphQL String

instance dataSourceNameShareReq :: DataSourceName ShareReq where
  dataSourceName _ = "share"

instance dataSourceShareReq :: MonadAff m => DataSource m ShareReq where
  fetch = doFetch

doFetch
  :: forall m a. MonadAff m
  => ShareReq -> ServiceT m a
doFetch (Create a b) = fromFn2 "create" a b
doFetch (CreateHistory a b) = fromFn2 "createHistory" a b
doFetch (SaveConfig a b) = fromFn2 "saveConfig" a b
doFetch (GetConfig a) = fromFn1 "getConfig" a
doFetch (GetList a) = fromFn1 "getList" a
doFetch (Statistic a) = fromFn1 "statistic" a
doFetch (Get a) = fromFn1 "get" a
doFetch (GetChildren a b) = fromFn2 "getChildren" a b
doFetch (GetHistory a b) = fromFn2 "getHistory" a b
doFetch (GetPatch a b) = fromFn2 "getPatch" a b
doFetch (GraphQL a) = fromFn1 "graphql" a

create
  :: forall m. MonadAff m => MonadThrow Error m
  => ShareName -> Name -> YuntanT m Json
create a = dataFetch <<< Create a

createHistory
  :: forall m. MonadAff m => MonadThrow Error m
  => Name -> Json -> YuntanT m Json
createHistory a = dataFetch <<< CreateHistory a

saveConfig
  :: forall m. MonadAff m => MonadThrow Error m
  => Key -> Value -> YuntanT m Unit
saveConfig a = dataFetch <<< SaveConfig a

getConfig
  :: forall m. MonadAff m => MonadThrow Error m
  => Key -> YuntanT m Json
getConfig = dataFetch <<< GetConfig

getList
  :: forall m. MonadAff m => MonadThrow Error m
  => Q -> YuntanT m Json
getList = dataFetch <<< GetList

statistic
  :: forall m. MonadAff m => MonadThrow Error m
  => Json -> YuntanT m Json
statistic = dataFetch <<< Statistic

get
  :: forall m. MonadAff m => MonadThrow Error m
  => Name -> YuntanT m Json
get = dataFetch <<< Get

getChildren
  :: forall m. MonadAff m => MonadThrow Error m
  => Name -> Q -> YuntanT m Json
getChildren a = dataFetch <<< GetChildren a

getHistory
  :: forall m. MonadAff m => MonadThrow Error m
  => Name -> Q -> YuntanT m Json
getHistory a = dataFetch <<< GetHistory a

getPatch
  :: forall m. MonadAff m => MonadThrow Error m
  => Name -> Json -> YuntanT m Json
getPatch a = dataFetch <<< GetPatch a

graphql
  :: forall m. MonadAff m => MonadThrow Error m
  => String -> YuntanT m Json
graphql = dataFetch <<< GraphQL
