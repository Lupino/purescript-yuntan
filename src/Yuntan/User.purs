module Yuntan.User
  ( initUser
  , Name
  , NameOrBid
  , NameOrUid
  , Password
  , Q (..)
  , defQ
  , Bind (..)

  , getList
  , create
  , get
  , remove
  , updateName
  , updatePassword
  , updateExtra
  , removeExtra
  , clearExtra
  , verifyPassword
  , createBind
  , getBind
  , removeBind
  , updateBindExtra
  , getBindListByUser
  , getBindListByService
  , getBindListByUserAndService
  , getListByGroup
  , createGroup
  , removeGroup
  , graphql
  , graphqlByUser
  , graphqlByBind
  , graphqlByService
  , configSet
  , configGet
  , configUserExtra
  , configBindExtra
  , getUserExtra
  , getBindExtra
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
type NameOrUid = String
type NameOrBid = String
type Password = String

type Q = { from :: Int, size :: Int }

defQ :: Q
defQ = { from: 0, size: 10 }

type Bind = {service :: String, name :: String, extra :: Json}

initUser :: forall opts. opts -> Effect Service
initUser = initService "user" "UserService"

data UserReq =
    GetList Q
  | Create Name Password
  | Get NameOrUid
  | Remove NameOrUid
  | UpdateName NameOrUid String
  | UpdatePassword NameOrUid Password
  | UpdateExtra NameOrUid Json
  | RemoveExtra NameOrUid Json
  | ClearExtra NameOrUid
  | VerifyPassword NameOrUid Password
  | CreateBind NameOrUid Bind
  | GetBind Name
  | RemoveBind NameOrBid
  | UpdateBindExtra NameOrBid Json
  | GetBindListByUser NameOrUid Q
  | GetBindListByService String Q
  | GetBindListByUserAndService NameOrUid String Q
  | GetListByGroup String Q
  | CreateGroup NameOrUid String
  | RemoveGroup NameOrUid String
  | GraphQL String
  | GraphQLByUser NameOrUid String
  | GraphQLByBind Name String
  | GraphQLByService String String
  | ConfigSet String Json
  | ConfigGet String

instance dataSourceNameUserReq :: DataSourceName UserReq where
  dataSourceName _ = "user"

instance dataSourceUserReq :: MonadAff m => DataSource m UserReq where
  fetch = doFetch

doFetch
  :: forall m a. MonadAff m
  => UserReq -> ServiceT m a

doFetch (GetList q) = importFn1 "getList" q
doFetch (Create n p) = importFn1 "create" {username: n, passwd: p}
doFetch (Get n) = importFn1 "get" n
doFetch (Remove n) = importFn1 "remove" n
doFetch (UpdateName n u) = importFn2 "updateName" n u
doFetch (UpdatePassword n p) = importFn2 "updatePassword" n p
doFetch (UpdateExtra n e) = importFn2 "updateExtra" n e
doFetch (RemoveExtra n e) = importFn2 "removeExtra" n e
doFetch (ClearExtra n) = importFn1 "clearExtra" n
doFetch (VerifyPassword n p) = importFn2 "verifyPassword" n p
doFetch (CreateBind n b) = importFn2 "createBind" n b
doFetch (GetBind n) = importFn1 "getBind" n
doFetch (RemoveBind n) = importFn1 "removeBind" n
doFetch (UpdateBindExtra b e) = importFn2 "updateBindExtra" b e
doFetch (GetBindListByUser n q) = importFn2 "getBindListByUser" n q
doFetch (GetBindListByService n q) = importFn2 "getBindListByService" n q
doFetch (GetBindListByUserAndService n s q) = importFn3 "getBindListByUserAndService" n s q
doFetch (GetListByGroup g q) = importFn2 "getListByGroup" g q
doFetch (CreateGroup n g) = importFn2 "createGroup" n g
doFetch (RemoveGroup n g) = importFn2 "removeGroup" n g
doFetch (GraphQL ql) = importFn1 "graphql" ql
doFetch (GraphQLByUser n ql) = importFn2 "graphqlByUser" n ql
doFetch (GraphQLByBind n ql) = importFn2 "graphqlByBind" n ql
doFetch (GraphQLByService n ql) = importFn2 "graphqlByService" n ql
doFetch (ConfigSet k v) = importFn2 "configSet" k v
doFetch (ConfigGet k) = importFn1 "configGet" k

getList
  :: forall m. MonadAff m => MonadThrow Error m
  => Q -> YuntanT m Json
getList = dataFetch <<< GetList

create
  :: forall m. MonadAff m => MonadThrow Error m
  => Name -> Password -> YuntanT m Json
create n = dataFetch <<< Create n

get
  :: forall m. MonadAff m => MonadThrow Error m
  => NameOrUid -> YuntanT m Json
get = dataFetch <<< Get

remove
  :: forall m. MonadAff m => MonadThrow Error m
  => NameOrUid -> YuntanT m Unit
remove = dataFetch <<< Remove

updateName
  :: forall m. MonadAff m => MonadThrow Error m
  => NameOrUid -> Name -> YuntanT m Unit
updateName n = dataFetch <<< UpdateName n

updatePassword
  :: forall m. MonadAff m => MonadThrow Error m
  => NameOrUid -> Password -> YuntanT m Unit
updatePassword n = dataFetch <<< UpdatePassword n

updateExtra
  :: forall m. MonadAff m => MonadThrow Error m
  => NameOrUid -> Json -> YuntanT m Unit
updateExtra n = dataFetch <<< UpdateExtra n

removeExtra
  :: forall m. MonadAff m => MonadThrow Error m
  => NameOrUid -> Json -> YuntanT m Unit
removeExtra n = dataFetch <<< RemoveExtra n

clearExtra
  :: forall m. MonadAff m => MonadThrow Error m
  => NameOrUid -> YuntanT m Unit
clearExtra = dataFetch <<< ClearExtra

verifyPassword
  :: forall m. MonadAff m => MonadThrow Error m
  => NameOrUid -> Password -> YuntanT m Unit
verifyPassword n = dataFetch <<< VerifyPassword n
createBind
  :: forall m. MonadAff m => MonadThrow Error m
  => NameOrUid -> Bind -> YuntanT m Json
createBind n = dataFetch <<< CreateBind n
getBind
  :: forall m. MonadAff m => MonadThrow Error m
  => Name -> YuntanT m Json
getBind = dataFetch <<< GetBind

removeBind
  :: forall m. MonadAff m => MonadThrow Error m
  => Name -> YuntanT m Unit
removeBind = dataFetch <<< RemoveBind

updateBindExtra
  :: forall m. MonadAff m => MonadThrow Error m
  => NameOrBid -> Json -> YuntanT m Unit
updateBindExtra n = dataFetch <<< UpdateBindExtra n

getBindListByUser
  :: forall m. MonadAff m => MonadThrow Error m
  => NameOrUid -> Q -> YuntanT m Json
getBindListByUser n = dataFetch <<< GetBindListByUser n

getBindListByService
  :: forall m. MonadAff m => MonadThrow Error m
   => String -> Q -> YuntanT m Json
getBindListByService s = dataFetch <<< GetBindListByService s

getBindListByUserAndService
  :: forall m. MonadAff m => MonadThrow Error m
   => NameOrUid -> String -> Q -> YuntanT m Json
getBindListByUserAndService n s = dataFetch <<< GetBindListByUserAndService n s

getListByGroup
  :: forall m. MonadAff m => MonadThrow Error m
   => String -> Q -> YuntanT m Json
getListByGroup g = dataFetch <<< GetListByGroup g

createGroup
  :: forall m. MonadAff m => MonadThrow Error m
  => NameOrUid -> String -> YuntanT m Unit
createGroup n = dataFetch <<< CreateGroup n

removeGroup
  :: forall m. MonadAff m => MonadThrow Error m
  => NameOrUid -> String -> YuntanT m Unit
removeGroup n = dataFetch <<< RemoveGroup n

graphql
  :: forall m. MonadAff m => MonadThrow Error m
  => String -> YuntanT m Json
graphql = dataFetch <<< GraphQL

graphqlByUser
  :: forall m. MonadAff m => MonadThrow Error m
  => NameOrUid -> String -> YuntanT m Json
graphqlByUser n = dataFetch <<< GraphQLByUser n

graphqlByBind
  :: forall m. MonadAff m => MonadThrow Error m
  => Name -> String -> YuntanT m Json
graphqlByBind n = dataFetch <<< GraphQLByBind n

graphqlByService
  :: forall m. MonadAff m => MonadThrow Error m
  => String -> String -> YuntanT m Json
graphqlByService s = dataFetch <<< GraphQLByService s

configSet
  :: forall m. MonadAff m => MonadThrow Error m
  => String -> Json -> YuntanT m Unit
configSet k = dataFetch <<< ConfigSet k

configGet
  :: forall m. MonadAff m => MonadThrow Error m
  => String -> YuntanT m Json
configGet = dataFetch <<< ConfigGet

configUserExtra
  :: forall m. MonadAff m => MonadThrow Error m
  => Json -> YuntanT m Unit
configUserExtra = configSet "user-extra"

getUserExtra
  :: forall m. MonadAff m => MonadThrow Error m
  => YuntanT m Json
getUserExtra = configGet "user-extra"

configBindExtra
  :: forall m. MonadAff m => MonadThrow Error m
  => Json -> YuntanT m Unit
configBindExtra = configSet "bind-extra"

getBindExtra
  :: forall m. MonadAff m => MonadThrow Error m
  => YuntanT m Json
getBindExtra = configGet "bind-extra"
