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
import Yuntan.Trans (class DataSourceName, class DataSource, ServiceT, YuntanT, dataFetch, initService, Service)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Aff.Class (class MonadAff)
import Yuntan.Utils (importFn1, importFn2)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Error.Class (class MonadThrow)

type Name = String
type ShareName = String
type Key = String
type Value = String


type Q = { from :: Int, size :: Int }

defQ :: Q
defQ = { from: 0, size: 10 }

initShare :: forall opts eff. opts -> Eff eff Service
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

instance dataSourceShareReq :: (MonadAff eff m, MonadEff eff m) => DataSource m ShareReq where
  fetch = doFetch

doFetch
  :: forall eff m a. MonadAff eff m => MonadEff eff m
  => ShareReq -> ServiceT m a
doFetch (Create a b) = importFn2 "create" a b
doFetch (CreateHistory a b) = importFn2 "createHistory" a b
doFetch (SaveConfig a b) = importFn2 "saveConfig" a b
doFetch (GetConfig a) = importFn1 "getConfig" a
doFetch (GetList a) = importFn1 "getList" a
doFetch (Statistic a) = importFn1 "statistic" a
doFetch (Get a) = importFn1 "get" a
doFetch (GetChildren a b) = importFn2 "getChildren" a b
doFetch (GetHistory a b) = importFn2 "getHistory" a b
doFetch (GetPatch a b) = importFn2 "getPatch" a b
doFetch (GraphQL a) = importFn1 "graphql" a

create
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => ShareName -> Name -> YuntanT m Json
create a = dataFetch <<< Create a

createHistory
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => Name -> Json -> YuntanT m Json
createHistory a = dataFetch <<< CreateHistory a

saveConfig
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => Key -> Value -> YuntanT m Unit
saveConfig a = dataFetch <<< SaveConfig a

getConfig
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => Key -> YuntanT m Json
getConfig = dataFetch <<< GetConfig

getList
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => Q -> YuntanT m Json
getList = dataFetch <<< GetList

statistic
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => Json -> YuntanT m Json
statistic = dataFetch <<< Statistic

get
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => Name -> YuntanT m Json
get = dataFetch <<< Get

getChildren
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => Name -> Q -> YuntanT m Json
getChildren a = dataFetch <<< GetChildren a

getHistory
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => Name -> Q -> YuntanT m Json
getHistory a = dataFetch <<< GetHistory a

getPatch
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => Name -> Json -> YuntanT m Json
getPatch a = dataFetch <<< GetPatch a

graphql
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => String -> YuntanT m Json
graphql = dataFetch <<< GraphQL
