module Yuntan.Article
  ( Q (..)
  , defQ

  , initArticle

  , saveFile
  , getFile
  , create
  , update
  , updateCover
  , removeCover
  , updateExtra
  , removeExtra
  , clearExtra
  , remove
  , get
  , exists
  , getList
  , createTag
  , getTag
  , addArticleTag
  , removeArticleTag
  , updateTag
  , getTagByName
  , createTimeline
  , removeTimeline
  , getTimelineList
  , saveTimelineMeta
  , getTimelineMeta
  , removeTimelineMeta
  , graphql

  ) where

import Prelude (Unit, (<<<))
import Data.Argonaut.Core (Json)
import Yuntan.Trans (class DataSourceName, class DataSource, ServiceT, YuntanT, dataFetch, initService, Service)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Aff.Class (class MonadAff)
import Yuntan.Utils (importFn1, importFn2, importFn3)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Error.Class (class MonadThrow)

type Q = { from :: Int, size :: Int }

type Timeline = String
type FileKey = String
type Bucket = String

type Meta = {title :: String, summary :: String}

type AID = Int
type Tag = String
type TID = Int
type FID = Int
type FromUrl = String

defQ :: Q
defQ = { from: 0, size: 10 }

initArticle :: forall opts eff. opts -> Eff eff Service
initArticle = initService "article" "ArticleService"

data ArticleReq =
    SaveFile FileKey Bucket Json
  | GetFile FileKey
  | Create Json
  | Update AID Json
  | UpdateCover AID FID
  | RemoveCover AID
  | UpdateExtra AID Json
  | RemoveExtra AID Json
  | ClearExtra AID
  | Remove AID
  | Get AID
  | Exists FromUrl
  | GetList Q
  | CreateTag Tag
  | GetTag TID
  | AddArticleTag AID Tag
  | RemoveArticleTag AID Tag
  | UpdateTag TID Tag
  | GetTagByName Tag
  | CreateTimeline Timeline AID
  | RemoveTimeline Timeline AID
  | GetTimelineList Timeline Q
  | SaveTimelineMeta Timeline Meta
  | GetTimelineMeta Timeline
  | RemoveTimelineMeta Timeline
  | GraphQL String

instance dataSourceNameArticleReq :: DataSourceName ArticleReq where
  dataSourceName _ = "article"

instance dataSourceArticleReq :: (MonadAff eff m, MonadEff eff m) => DataSource m ArticleReq where
  fetch = doFetch

doFetch
  :: forall eff m a. MonadAff eff m => MonadEff eff m
  => ArticleReq -> ServiceT m a

doFetch (SaveFile a b c) = importFn3 "saveFile" a b c
doFetch (GetFile a) = importFn1 "getFile" a
doFetch (Create a) = importFn1 "create" a
doFetch (Update a b) = importFn2 "update" a b
doFetch (UpdateCover a b) = importFn2 "updateCover" a b
doFetch (RemoveCover a) = importFn1 "removeCover" a
doFetch (UpdateExtra a b) = importFn2 "updateExtra" a b
doFetch (RemoveExtra a b) = importFn2 "removeExtra" a b
doFetch (ClearExtra a) = importFn1 "clearExtra" a
doFetch (Remove a) = importFn1 "remove" a
doFetch (Get a) = importFn1 "get" a
doFetch (Exists a) = importFn1 "exists" a
doFetch (GetList a) = importFn1 "getList" a
doFetch (CreateTag a) = importFn1 "createTag" a
doFetch (GetTag a) = importFn1 "getTag" a
doFetch (AddArticleTag a b) = importFn2 "addArticleTag" a b
doFetch (RemoveArticleTag a b) = importFn2 "removeArticleTag" a b
doFetch (UpdateTag a b) = importFn2 "updateTag" a b
doFetch (GetTagByName a) = importFn1 "getTagByName" a
doFetch (CreateTimeline a b) = importFn2 "createTimeline" a b
doFetch (RemoveTimeline a b) = importFn2 "removeTimeline" a b
doFetch (GetTimelineList a b) = importFn2 "getTimelineList" a b
doFetch (SaveTimelineMeta a b) = importFn2 "saveTimelineMeta" a b
doFetch (GetTimelineMeta a) = importFn1 "getTimelineMeta" a
doFetch (RemoveTimelineMeta a) = importFn1 "removeTimelineMeta" a
doFetch (GraphQL a) = importFn1 "graphQL" a

saveFile
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => FileKey -> Bucket -> Json -> YuntanT m Json
saveFile a b = dataFetch <<< SaveFile a b

getFile
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => FileKey -> YuntanT m Json
getFile = dataFetch <<< GetFile

create
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => Json -> YuntanT m Json
create = dataFetch <<< Create

update
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => AID -> Json -> YuntanT m Json
update a = dataFetch <<< Update a

updateCover
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => AID -> FID -> YuntanT m Unit
updateCover a = dataFetch <<< UpdateCover a

removeCover
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => AID -> YuntanT m Unit
removeCover = dataFetch <<< RemoveCover

updateExtra
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => AID -> Json -> YuntanT m Unit
updateExtra a = dataFetch <<< UpdateExtra a

removeExtra
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => AID -> Json -> YuntanT m Unit
removeExtra a = dataFetch <<< RemoveExtra a

clearExtra
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => AID -> YuntanT m Unit
clearExtra = dataFetch <<< ClearExtra

remove
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => AID -> YuntanT m Unit
remove = dataFetch <<< Remove

get
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => AID -> YuntanT m Json
get = dataFetch <<< Get

exists
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => FromUrl -> YuntanT m AID
exists = dataFetch <<< Exists

getList
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => Q -> YuntanT m Json
getList = dataFetch <<< GetList

createTag
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => Tag -> YuntanT m Json
createTag = dataFetch <<< CreateTag

getTag
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => TID -> YuntanT m Json
getTag = dataFetch <<< GetTag

addArticleTag
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => AID -> Tag -> YuntanT m Unit
addArticleTag a = dataFetch <<< AddArticleTag a

removeArticleTag
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => AID -> Tag -> YuntanT m Unit
removeArticleTag a = dataFetch <<< RemoveArticleTag a

updateTag
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => TID -> Tag -> YuntanT m Unit
updateTag a = dataFetch <<< UpdateTag a

getTagByName
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => Tag -> YuntanT m Json
getTagByName = dataFetch <<< GetTagByName

createTimeline
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => Timeline -> AID -> YuntanT m Unit
createTimeline a = dataFetch <<< CreateTimeline a

removeTimeline
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => Timeline -> AID -> YuntanT m Unit
removeTimeline a = dataFetch <<< RemoveTimeline a

getTimelineList
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => Timeline -> Q -> YuntanT m Json
getTimelineList a = dataFetch <<< GetTimelineList a

saveTimelineMeta
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => Timeline -> Meta -> YuntanT m Unit
saveTimelineMeta a = dataFetch <<< SaveTimelineMeta a

getTimelineMeta
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => Timeline -> YuntanT m Meta
getTimelineMeta = dataFetch <<< GetTimelineMeta

removeTimelineMeta
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => Timeline -> YuntanT m Unit
removeTimelineMeta = dataFetch <<< RemoveTimelineMeta

graphql
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => String -> YuntanT m Json
graphql = dataFetch <<< GraphQL
