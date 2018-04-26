module Yuntan.Cart
  ( Q (..)
  , defQ
  , initCart

  , addProduct
  , getCart
  , removeProduct
  , createOrder
  , updateOrderStatus
  , updateOrderStatusByUserName
  , updateOrderBody
  , updateOrderAmount
  , getOrderList
  , getOrderListByStatus
  , getOrderListByUserName
  , getOrderListByUserNameAndStatus
  , getOrder
  , removeOrder
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

type OrderIdOrSn = String
type UserName = String
type ProductId = String

type Q = { from :: Int, size :: Int }
defQ :: Q
defQ = { from: 0, size: 10 }

initCart :: forall opts eff. opts -> Eff eff Service
initCart = initService "cart" "CartService"

data CartReq =
    AddProduct UserName Json
  | GetCart UserName
  | RemoveProduct UserName ProductId
  | CreateOrder Json
  | UpdateOrderStatus OrderIdOrSn String
  | UpdateOrderStatusByUserName UserName OrderIdOrSn String
  | UpdateOrderBody OrderIdOrSn Json
  | UpdateOrderAmount OrderIdOrSn Int
  | GetOrderList Q
  | GetOrderListByStatus String Q
  | GetOrderListByUserName UserName Q
  | GetOrderListByUserNameAndStatus UserName String Q
  | GetOrder OrderIdOrSn
  | RemoveOrder OrderIdOrSn

instance dataSourceNameCartReq :: DataSourceName CartReq where
  dataSourceName _ = "cart"

instance dataSourceCartReq :: (MonadAff eff m, MonadEff eff m) => DataSource m CartReq where
  fetch = doFetch

doFetch
  :: forall eff m a. MonadAff eff m => MonadEff eff m
  => CartReq -> ServiceT m a
doFetch (AddProduct a b) = importFn2 "addProduct" a b
doFetch (GetCart a) = importFn1 "getCart" a
doFetch (RemoveProduct a b) = importFn2 "removeProduct" a b
doFetch (CreateOrder a) = importFn1 "createOrder" a
doFetch (UpdateOrderStatus a b) = importFn2 "updateOrderStatus" a b
doFetch (UpdateOrderStatusByUserName a b c) = importFn3 "updateOrderStatusByUserName" a b c
doFetch (UpdateOrderBody a b) = importFn2 "updateOrderBody" a b
doFetch (UpdateOrderAmount a b) = importFn2 "updateOrderAmount" a b
doFetch (GetOrderList a) = importFn1 "getOrderList" a
doFetch (GetOrderListByStatus a b) = importFn2 "getOrderListByStatus" a b
doFetch (GetOrderListByUserName a b) = importFn2 "getOrderListByUserName" a b
doFetch (GetOrderListByUserNameAndStatus a b c) = importFn3 "getOrderListByUserNameAndStatus" a b c
doFetch (GetOrder a) = importFn1 "getOrder" a
doFetch (RemoveOrder a) = importFn1 "removeOrder" a

addProduct
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => UserName -> Json -> YuntanT m Unit
addProduct a = dataFetch <<< AddProduct a

getCart
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => UserName -> YuntanT m Json
getCart = dataFetch <<< GetCart

removeProduct
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => UserName -> ProductId -> YuntanT m Unit
removeProduct a = dataFetch <<< RemoveProduct a

createOrder
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => Json -> YuntanT m Json
createOrder = dataFetch <<< CreateOrder

updateOrderStatus
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => OrderIdOrSn -> String -> YuntanT m Unit
updateOrderStatus a = dataFetch <<< UpdateOrderStatus a

updateOrderStatusByUserName
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => UserName -> OrderIdOrSn -> String -> YuntanT m Unit
updateOrderStatusByUserName a b = dataFetch <<< UpdateOrderStatusByUserName a b

updateOrderBody
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => OrderIdOrSn -> Json -> YuntanT m Unit
updateOrderBody a = dataFetch <<< UpdateOrderBody a

updateOrderAmount
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => OrderIdOrSn -> Int -> YuntanT m Unit
updateOrderAmount a = dataFetch <<< UpdateOrderAmount a

getOrderList
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => Q -> YuntanT m Json
getOrderList = dataFetch <<< GetOrderList

getOrderListByStatus
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => String -> Q -> YuntanT m Json
getOrderListByStatus a = dataFetch <<< GetOrderListByStatus a

getOrderListByUserName
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => UserName -> Q -> YuntanT m Json
getOrderListByUserName a = dataFetch <<< GetOrderListByUserName a

getOrderListByUserNameAndStatus
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => UserName -> String -> Q -> YuntanT m Json
getOrderListByUserNameAndStatus a b = dataFetch <<< GetOrderListByUserNameAndStatus a b

getOrder
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => OrderIdOrSn -> YuntanT m Json
getOrder = dataFetch <<< GetOrder

removeOrder
  :: forall eff m. MonadAff eff m => MonadEff eff m => MonadThrow Error m
  => OrderIdOrSn -> YuntanT m Unit
removeOrder = dataFetch <<< RemoveOrder
