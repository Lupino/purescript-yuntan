module Yuntan.Search
  ( initSearch
  , createIndex
  , getIndex
  , deleteIndex
  , listIndexes
  , docIndex
  , docCount
  , docGet
  , docDelete
  , search
  , listFields
  , debug
  , alias
  ) where

import Prelude (Unit, (<<<))
import Data.Argonaut.Core (Json)
import Yuntan.Internal.Trans (class DataSourceName, class DataSource, ServiceT, YuntanT, dataFetch, Service)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Yuntan.Internal.Utils (fromFn0, fromFn1, fromFn2, fromFn3)
import Effect.Exception (Error)
import Control.Monad.Error.Class (class MonadThrow)
import Yuntan.Helper (initService)

type IndexName = String
type DocID = String

initSearch :: forall opts. opts -> Effect Service
initSearch = initService "search" "SearchService"

data SearchReq =
    CreateIndex IndexName Json
  | GetIndex IndexName
  | DeleteIndex IndexName
  | ListIndexes
  | DocIndex IndexName DocID Json
  | DocCount IndexName
  | DocGet IndexName DocID
  | DocDelete IndexName DocID
  | Search IndexName Json
  | ListFields IndexName
  | Debug IndexName DocID
  | Alias Json

instance dataSourceNameSearchReq :: DataSourceName SearchReq where
  dataSourceName _ = "search"

instance dataSourceSearchReq :: MonadAff m => DataSource m SearchReq where
  fetch = doFetch

doFetch
  :: forall m a. MonadAff m
  => SearchReq -> ServiceT m a
doFetch (CreateIndex a b) = fromFn2 "createIndex" a b
doFetch (GetIndex a) = fromFn1 "getIndex" a
doFetch (DeleteIndex a) = fromFn1 "deleteIndex" a
doFetch (ListIndexes) = fromFn0 "listIndexes"
doFetch (DocIndex a b c) = fromFn3 "docIndex" a b c
doFetch (DocCount a) = fromFn1 "docCount" a
doFetch (DocGet a b) = fromFn2 "docGet" a b
doFetch (DocDelete a b) = fromFn2 "docDelete" a b
doFetch (Search a b) = fromFn2 "search" a b
doFetch (ListFields a) = fromFn1 "listFields" a
doFetch (Debug a b) = fromFn2 "debug" a b
doFetch (Alias a) = fromFn1 "alias" a

createIndex
  :: forall m. MonadAff m => MonadThrow Error m
  => IndexName -> Json -> YuntanT m Unit
createIndex a = dataFetch <<< CreateIndex a

getIndex
  :: forall m. MonadAff m => MonadThrow Error m
  => IndexName -> YuntanT m Json
getIndex = dataFetch <<< GetIndex

deleteIndex
  :: forall m. MonadAff m => MonadThrow Error m
  => IndexName -> YuntanT m Unit
deleteIndex = dataFetch <<< DeleteIndex

listIndexes
  :: forall m. MonadAff m => MonadThrow Error m
  => YuntanT m Json
listIndexes = dataFetch ListIndexes

docIndex
  :: forall m. MonadAff m => MonadThrow Error m
  => IndexName -> DocID -> Json -> YuntanT m Unit
docIndex a b = dataFetch <<< DocIndex a b

docCount
  :: forall m. MonadAff m => MonadThrow Error m
  => IndexName -> YuntanT m Int
docCount = dataFetch <<< DocCount

docGet
  :: forall m. MonadAff m => MonadThrow Error m
  => IndexName -> DocID -> YuntanT m Json
docGet a = dataFetch <<< DocGet a

docDelete
  :: forall m. MonadAff m => MonadThrow Error m
  => IndexName -> DocID -> YuntanT m Unit
docDelete a = dataFetch <<< DocDelete a

search
  :: forall m. MonadAff m => MonadThrow Error m
  => IndexName -> Json -> YuntanT m Json
search a = dataFetch <<< Search a

listFields
  :: forall m. MonadAff m => MonadThrow Error m
  => IndexName -> YuntanT m Json
listFields = dataFetch <<< ListFields

debug
  :: forall m. MonadAff m => MonadThrow Error m
  => IndexName -> DocID -> YuntanT m Json
debug a = dataFetch <<< Debug a

alias
  :: forall m. MonadAff m => MonadThrow Error m
  => Json -> YuntanT m Unit
alias = dataFetch <<< Alias
