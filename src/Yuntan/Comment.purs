module Yuntan.Comment
  ( Q (..)
  , defQ
  , initComment

  , create
  , getList
  , get
  , remove
  , removeList
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

type For = String
type CID = String

type Q = { from :: Int, size :: Int }
defQ :: Q
defQ = { from: 0, size: 10 }

initComment :: forall opts. opts -> Effect Service
initComment = initService "comment" "CommentService"

data CommentReq =
    Create For Json
  | GetList For Q
  | Get CID
  | Remove CID
  | RemoveList For

instance dataSourceNameCommentReq :: DataSourceName CommentReq where
  dataSourceName _ = "comment"

instance dataSourceCommentReq :: MonadAff m => DataSource m CommentReq where
  fetch = doFetch

doFetch
  :: forall m a. MonadAff m
  => CommentReq -> ServiceT m a

doFetch (Create a b) = fromFn2 "create" a b
doFetch (GetList a b) = fromFn2 "getList" a b
doFetch (Get a) = fromFn1 "get" a
doFetch (Remove a) = fromFn1 "remove" a
doFetch (RemoveList a) = fromFn1 "removeList" a

create
  :: forall m. MonadAff m => MonadThrow Error m
  => For -> Json -> YuntanT m Json
create a = dataFetch <<< Create a

getList
  :: forall m. MonadAff m => MonadThrow Error m
  => For -> Q -> YuntanT m Json
getList a = dataFetch <<< GetList a

get
  :: forall m. MonadAff m => MonadThrow Error m
  => CID -> YuntanT m Json
get = dataFetch <<< Get

remove
  :: forall m. MonadAff m => MonadThrow Error m
  => CID -> YuntanT m Unit
remove = dataFetch <<< Remove

removeList
  :: forall m. MonadAff m => MonadThrow Error m
  => For -> YuntanT m Unit
removeList = dataFetch <<< RemoveList
