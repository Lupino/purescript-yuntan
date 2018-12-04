module Yuntan.Device
  ( initDevice
  , Q (..)
  , defQ

  , create
  , updateToken
  , updateType
  , updateMeta
  , getList
  , getListByUser
  , remove
  , get
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

type UuidOrToken = String
type UUID = String
type Token = String
type Type = String

type Q = { from :: Int, size :: Int, type :: String }

defQ :: Q
defQ = { from: 0, size: 10, type: "" }

initDevice :: forall opts. opts -> Effect Service
initDevice = initService "device" "DeviceService"

data DeviceReq =
  Create String Token Type
  | UpdateToken UuidOrToken Token
  | UpdateType UuidOrToken Type
  | UpdateMeta UuidOrToken Json
  | GetList Q
  | GetListByUser String Q
  | Remove UuidOrToken
  | Get UuidOrToken

instance dataSourceNameDeviceReq :: DataSourceName DeviceReq where
  dataSourceName _ = "device"

instance dataSourceDeviceReq :: MonadAff m => DataSource m DeviceReq where
  fetch = doFetch

doFetch
  :: forall m a. MonadAff m
  => DeviceReq -> ServiceT m a

doFetch (Create n token tp) = fromFn1 "create" {username: n, token: token, type: tp}
doFetch (UpdateToken t token) = fromFn2 "updateToken" t token
doFetch (UpdateType t tp) = fromFn2 "updateType" t tp
doFetch (UpdateMeta t meta) = fromFn2 "updateMeta" t meta
doFetch (GetList q) = fromFn1 "getList" q
doFetch (GetListByUser u q) = fromFn2 "getListByUser" u q
doFetch (Remove n) = fromFn1 "remove" n
doFetch (Get n) = fromFn1 "get" n

create
  :: forall m. MonadAff m => MonadThrow Error m
  => String -> Token -> Type -> YuntanT m Json
create n token = dataFetch <<< Create n token

updateToken
  :: forall m. MonadAff m => MonadThrow Error m
  => UuidOrToken -> Token -> YuntanT m Unit
updateToken n = dataFetch <<< UpdateToken n

updateType
  :: forall m. MonadAff m => MonadThrow Error m
  => UuidOrToken -> Type -> YuntanT m Unit
updateType n = dataFetch <<< UpdateType n

updateMeta
  :: forall m. MonadAff m => MonadThrow Error m
  => UuidOrToken -> Json -> YuntanT m Unit
updateMeta n = dataFetch <<< UpdateMeta n

getList
  :: forall m. MonadAff m => MonadThrow Error m
  => Q -> YuntanT m Json
getList = dataFetch <<< GetList

getListByUser
  :: forall m. MonadAff m => MonadThrow Error m
  => String -> Q -> YuntanT m Json
getListByUser n = dataFetch <<< GetListByUser n

remove
  :: forall m. MonadAff m => MonadThrow Error m
  => UuidOrToken -> YuntanT m Unit
remove = dataFetch <<< Remove

get
  :: forall m. MonadAff m => MonadThrow Error m
  => UuidOrToken -> YuntanT m Json
get = dataFetch <<< Get
