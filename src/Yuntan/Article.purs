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

  , configSet
  , configGet
  , configArticleExtra
  , getArticleExtra
  ) where

import Prelude (Unit, (<<<))
import Data.Argonaut.Core (Json)
import Yuntan.Internal.Trans (class DataSourceName, class DataSource, ServiceT, YuntanT, dataFetch, Service)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Yuntan.Internal.Utils (fromFn1, fromFn2, fromFn3)
import Effect.Exception (Error)
import Control.Monad.Error.Class (class MonadThrow)
import Yuntan.Helper (initService)

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

initArticle :: forall opts. opts -> Effect Service
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
  | ConfigSet String Json
  | ConfigGet String

instance dataSourceNameArticleReq :: DataSourceName ArticleReq where
  dataSourceName _ = "article"

instance dataSourceArticleReq :: MonadAff m => DataSource m ArticleReq where
  fetch = doFetch

doFetch
  :: forall m a. MonadAff m
  => ArticleReq -> ServiceT m a

doFetch (SaveFile a b c) = fromFn3 "saveFile" a b c
doFetch (GetFile a) = fromFn1 "getFile" a
doFetch (Create a) = fromFn1 "create" a
doFetch (Update a b) = fromFn2 "update" a b
doFetch (UpdateCover a b) = fromFn2 "updateCover" a b
doFetch (RemoveCover a) = fromFn1 "removeCover" a
doFetch (UpdateExtra a b) = fromFn2 "updateExtra" a b
doFetch (RemoveExtra a b) = fromFn2 "removeExtra" a b
doFetch (ClearExtra a) = fromFn1 "clearExtra" a
doFetch (Remove a) = fromFn1 "remove" a
doFetch (Get a) = fromFn1 "get" a
doFetch (Exists a) = fromFn1 "exists" a
doFetch (GetList a) = fromFn1 "getList" a
doFetch (CreateTag a) = fromFn1 "createTag" a
doFetch (GetTag a) = fromFn1 "getTag" a
doFetch (AddArticleTag a b) = fromFn2 "addArticleTag" a b
doFetch (RemoveArticleTag a b) = fromFn2 "removeArticleTag" a b
doFetch (UpdateTag a b) = fromFn2 "updateTag" a b
doFetch (GetTagByName a) = fromFn1 "getTagByName" a
doFetch (CreateTimeline a b) = fromFn2 "createTimeline" a b
doFetch (RemoveTimeline a b) = fromFn2 "removeTimeline" a b
doFetch (GetTimelineList a b) = fromFn2 "getTimelineList" a b
doFetch (SaveTimelineMeta a b) = fromFn2 "saveTimelineMeta" a b
doFetch (GetTimelineMeta a) = fromFn1 "getTimelineMeta" a
doFetch (RemoveTimelineMeta a) = fromFn1 "removeTimelineMeta" a
doFetch (GraphQL a) = fromFn1 "graphql" a
doFetch (ConfigSet k v) = fromFn2 "configSet" k v
doFetch (ConfigGet k) = fromFn1 "configGet" k

saveFile
  :: forall m. MonadAff m => MonadThrow Error m
  => FileKey -> Bucket -> Json -> YuntanT m Json
saveFile a b = dataFetch <<< SaveFile a b

getFile
  :: forall m. MonadAff m => MonadThrow Error m
  => FileKey -> YuntanT m Json
getFile = dataFetch <<< GetFile

create
  :: forall m. MonadAff m => MonadThrow Error m
  => Json -> YuntanT m Json
create = dataFetch <<< Create

update
  :: forall m. MonadAff m => MonadThrow Error m
  => AID -> Json -> YuntanT m Json
update a = dataFetch <<< Update a

updateCover
  :: forall m. MonadAff m => MonadThrow Error m
  => AID -> FID -> YuntanT m Unit
updateCover a = dataFetch <<< UpdateCover a

removeCover
  :: forall m. MonadAff m => MonadThrow Error m
  => AID -> YuntanT m Unit
removeCover = dataFetch <<< RemoveCover

updateExtra
  :: forall m. MonadAff m => MonadThrow Error m
  => AID -> Json -> YuntanT m Unit
updateExtra a = dataFetch <<< UpdateExtra a

removeExtra
  :: forall m. MonadAff m => MonadThrow Error m
  => AID -> Json -> YuntanT m Unit
removeExtra a = dataFetch <<< RemoveExtra a

clearExtra
  :: forall m. MonadAff m => MonadThrow Error m
  => AID -> YuntanT m Unit
clearExtra = dataFetch <<< ClearExtra

remove
  :: forall m. MonadAff m => MonadThrow Error m
  => AID -> YuntanT m Unit
remove = dataFetch <<< Remove

get
  :: forall m. MonadAff m => MonadThrow Error m
  => AID -> YuntanT m Json
get = dataFetch <<< Get

exists
  :: forall m. MonadAff m => MonadThrow Error m
  => FromUrl -> YuntanT m AID
exists = dataFetch <<< Exists

getList
  :: forall m. MonadAff m => MonadThrow Error m
  => Q -> YuntanT m Json
getList = dataFetch <<< GetList

createTag
  :: forall m. MonadAff m => MonadThrow Error m
  => Tag -> YuntanT m Json
createTag = dataFetch <<< CreateTag

getTag
  :: forall m. MonadAff m => MonadThrow Error m
  => TID -> YuntanT m Json
getTag = dataFetch <<< GetTag

addArticleTag
  :: forall m. MonadAff m => MonadThrow Error m
  => AID -> Tag -> YuntanT m Unit
addArticleTag a = dataFetch <<< AddArticleTag a

removeArticleTag
  :: forall m. MonadAff m => MonadThrow Error m
  => AID -> Tag -> YuntanT m Unit
removeArticleTag a = dataFetch <<< RemoveArticleTag a

updateTag
  :: forall m. MonadAff m => MonadThrow Error m
  => TID -> Tag -> YuntanT m Unit
updateTag a = dataFetch <<< UpdateTag a

getTagByName
  :: forall m. MonadAff m => MonadThrow Error m
  => Tag -> YuntanT m Json
getTagByName = dataFetch <<< GetTagByName

createTimeline
  :: forall m. MonadAff m => MonadThrow Error m
  => Timeline -> AID -> YuntanT m Unit
createTimeline a = dataFetch <<< CreateTimeline a

removeTimeline
  :: forall m. MonadAff m => MonadThrow Error m
  => Timeline -> AID -> YuntanT m Unit
removeTimeline a = dataFetch <<< RemoveTimeline a

getTimelineList
  :: forall m. MonadAff m => MonadThrow Error m
  => Timeline -> Q -> YuntanT m Json
getTimelineList a = dataFetch <<< GetTimelineList a

saveTimelineMeta
  :: forall m. MonadAff m => MonadThrow Error m
  => Timeline -> Meta -> YuntanT m Unit
saveTimelineMeta a = dataFetch <<< SaveTimelineMeta a

getTimelineMeta
  :: forall m. MonadAff m => MonadThrow Error m
  => Timeline -> YuntanT m Meta
getTimelineMeta = dataFetch <<< GetTimelineMeta

removeTimelineMeta
  :: forall m. MonadAff m => MonadThrow Error m
  => Timeline -> YuntanT m Unit
removeTimelineMeta = dataFetch <<< RemoveTimelineMeta

graphql
  :: forall m. MonadAff m => MonadThrow Error m
  => String -> YuntanT m Json
graphql = dataFetch <<< GraphQL

configSet
  :: forall m. MonadAff m => MonadThrow Error m
  => String -> Json -> YuntanT m Unit
configSet k = dataFetch <<< ConfigSet k

configGet
  :: forall m. MonadAff m => MonadThrow Error m
  => String -> YuntanT m Json
configGet = dataFetch <<< ConfigGet

configArticleExtra
  :: forall m. MonadAff m => MonadThrow Error m
  => Json -> YuntanT m Unit
configArticleExtra = configSet "article-extra"

getArticleExtra
  :: forall m. MonadAff m => MonadThrow Error m
  => YuntanT m Json
getArticleExtra = configGet "article-extra"
