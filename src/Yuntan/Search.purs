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
import Yuntan.Trans (class DataSourceName, class DataSource, ServiceT, YuntanT, dataFetch, initService, Service)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Aff.Class (class MonadAff)
import Yuntan.Utils (importFn0, importFn1, importFn2, importFn3)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Error.Class (class MonadThrow)

type IndexName = String
type DocID = String

initSearch :: forall opts eff. opts -> Eff eff Service
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

instance dataSourceSearchReq :: (MonadAff eff m, MonadEff eff m) => DataSource m SearchReq where
  fetch = doFetch

doFetch
  :: forall eff m a. MonadAff eff m => MonadEff eff m
  => SearchReq -> ServiceT m a
doFetch (CreateIndex a b) = importFn2 "createIndex" a b
doFetch (GetIndex a) = importFn1 "getIndex" a
doFetch (DeleteIndex a) = importFn1 "deleteIndex" a
doFetch (ListIndexes) = importFn0 "listIndexes"
doFetch (DocIndex a b c) = importFn3 "docIndex" a b c
doFetch (DocCount a) = importFn1 "docCount" a
doFetch (DocGet a b) = importFn2 "docGet" a b
doFetch (DocDelete a b) = importFn2 "docDelete" a b
doFetch (Search a b) = importFn2 "search" a b
doFetch (ListFields a) = importFn1 "listFields" a
doFetch (Debug a b) = importFn2 "debug" a b
doFetch (Alias a) = importFn1 "alias" a

createIndex
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => IndexName -> Json -> YuntanT m Unit
createIndex a = dataFetch <<< CreateIndex a

getIndex
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => IndexName -> YuntanT m Json
getIndex = dataFetch <<< GetIndex

deleteIndex
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => IndexName -> YuntanT m Unit
deleteIndex = dataFetch <<< DeleteIndex

listIndexes
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => YuntanT m Json
listIndexes = dataFetch ListIndexes

docIndex
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => IndexName -> DocID -> Json -> YuntanT m Unit
docIndex a b = dataFetch <<< DocIndex a b

docCount
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => IndexName -> YuntanT m Int
docCount = dataFetch <<< DocCount

docGet
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => IndexName -> DocID -> YuntanT m Json
docGet a = dataFetch <<< DocGet a

docDelete
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => IndexName -> DocID -> YuntanT m Unit
docDelete a = dataFetch <<< DocDelete a

search
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => IndexName -> Json -> YuntanT m Json
search a = dataFetch <<< Search a

listFields
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => IndexName -> YuntanT m Json
listFields = dataFetch <<< ListFields

debug
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => IndexName -> DocID -> YuntanT m Json
debug a = dataFetch <<< Debug a

alias
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => Json -> YuntanT m Unit
alias = dataFetch <<< Alias
