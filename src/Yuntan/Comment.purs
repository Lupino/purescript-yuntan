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
import Yuntan.Trans (class DataSourceName, class DataSource, ServiceT, YuntanT, dataFetch, initService, Service)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Aff.Class (class MonadAff)
import Yuntan.Utils (importFn1, importFn2)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Error.Class (class MonadThrow)

type For = String
type CID = String

type Q = { from :: Int, size :: Int }
defQ :: Q
defQ = { from: 0, size: 10 }

initComment :: forall opts eff. opts -> Eff eff Service
initComment = initService "comment" "CommentService"

data CommentReq =
    Create For Json
  | GetList For Q
  | Get CID
  | Remove CID
  | RemoveList For

instance dataSourceNameCommentReq :: DataSourceName CommentReq where
  dataSourceName _ = "comment"

instance dataSourceCommentReq :: (MonadAff eff m, MonadEff eff m) => DataSource m CommentReq where
  fetch = doFetch

doFetch
  :: forall eff m a. MonadAff eff m => MonadEff eff m
  => CommentReq -> ServiceT m a

doFetch (Create a b) = importFn2 "create" a b
doFetch (GetList a b) = importFn2 "getList" a b
doFetch (Get a) = importFn1 "get" a
doFetch (Remove a) = importFn1 "remove" a
doFetch (RemoveList a) = importFn1 "removeList" a

create
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => For -> Json -> YuntanT m Json
create a = dataFetch <<< Create a

getList
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => For -> Q -> YuntanT m Json
getList a = dataFetch <<< GetList a

get
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => CID -> YuntanT m Json
get = dataFetch <<< Get

remove
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => CID -> YuntanT m Unit
remove = dataFetch <<< Remove

removeList
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => For -> YuntanT m Unit
removeList = dataFetch <<< RemoveList
