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
import Yuntan.Internal.Trans (class DataSourceName, class DataSource, ServiceT, YuntanT, dataFetch, Service)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Yuntan.Internal.Utils (fromFn1, fromFn2, fromFn3)
import Effect.Exception (Error)
import Control.Monad.Error.Class (class MonadThrow)
import Yuntan.Helper (initService)

type OrderIdOrSn = String
type UserName = String
type ProductId = String

type Q = { from :: Int, size :: Int }
defQ :: Q
defQ = { from: 0, size: 10 }

initCart :: forall opts. opts -> Effect Service
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

instance dataSourceCartReq :: MonadAff m => DataSource m CartReq where
  fetch = doFetch

doFetch
  :: forall m a. MonadAff m
  => CartReq -> ServiceT m a
doFetch (AddProduct a b) = fromFn2 "addProduct" a b
doFetch (GetCart a) = fromFn1 "getCart" a
doFetch (RemoveProduct a b) = fromFn2 "removeProduct" a b
doFetch (CreateOrder a) = fromFn1 "createOrder" a
doFetch (UpdateOrderStatus a b) = fromFn2 "updateOrderStatus" a b
doFetch (UpdateOrderStatusByUserName a b c) = fromFn3 "updateOrderStatusByUserName" a b c
doFetch (UpdateOrderBody a b) = fromFn2 "updateOrderBody" a b
doFetch (UpdateOrderAmount a b) = fromFn2 "updateOrderAmount" a b
doFetch (GetOrderList a) = fromFn1 "getOrderList" a
doFetch (GetOrderListByStatus a b) = fromFn2 "getOrderListByStatus" a b
doFetch (GetOrderListByUserName a b) = fromFn2 "getOrderListByUserName" a b
doFetch (GetOrderListByUserNameAndStatus a b c) = fromFn3 "getOrderListByUserNameAndStatus" a b c
doFetch (GetOrder a) = fromFn1 "getOrder" a
doFetch (RemoveOrder a) = fromFn1 "removeOrder" a

addProduct
  :: forall m. MonadAff m => MonadThrow Error m
  => UserName -> Json -> YuntanT m Unit
addProduct a = dataFetch <<< AddProduct a

getCart
  :: forall m. MonadAff m => MonadThrow Error m
  => UserName -> YuntanT m Json
getCart = dataFetch <<< GetCart

removeProduct
  :: forall m. MonadAff m => MonadThrow Error m
  => UserName -> ProductId -> YuntanT m Unit
removeProduct a = dataFetch <<< RemoveProduct a

createOrder
  :: forall m. MonadAff m => MonadThrow Error m
  => Json -> YuntanT m Json
createOrder = dataFetch <<< CreateOrder

updateOrderStatus
  :: forall m. MonadAff m => MonadThrow Error m
  => OrderIdOrSn -> String -> YuntanT m Unit
updateOrderStatus a = dataFetch <<< UpdateOrderStatus a

updateOrderStatusByUserName
  :: forall m. MonadAff m => MonadThrow Error m
  => UserName -> OrderIdOrSn -> String -> YuntanT m Unit
updateOrderStatusByUserName a b = dataFetch <<< UpdateOrderStatusByUserName a b

updateOrderBody
  :: forall m. MonadAff m => MonadThrow Error m
  => OrderIdOrSn -> Json -> YuntanT m Unit
updateOrderBody a = dataFetch <<< UpdateOrderBody a

updateOrderAmount
  :: forall m. MonadAff m => MonadThrow Error m
  => OrderIdOrSn -> Int -> YuntanT m Unit
updateOrderAmount a = dataFetch <<< UpdateOrderAmount a

getOrderList
  :: forall m. MonadAff m => MonadThrow Error m
  => Q -> YuntanT m Json
getOrderList = dataFetch <<< GetOrderList

getOrderListByStatus
  :: forall m. MonadAff m => MonadThrow Error m
  => String -> Q -> YuntanT m Json
getOrderListByStatus a = dataFetch <<< GetOrderListByStatus a

getOrderListByUserName
  :: forall m. MonadAff m => MonadThrow Error m
  => UserName -> Q -> YuntanT m Json
getOrderListByUserName a = dataFetch <<< GetOrderListByUserName a

getOrderListByUserNameAndStatus
  :: forall m. MonadAff m => MonadThrow Error m
  => UserName -> String -> Q -> YuntanT m Json
getOrderListByUserNameAndStatus a b = dataFetch <<< GetOrderListByUserNameAndStatus a b

getOrder
  :: forall m. MonadAff m => MonadThrow Error m
  => OrderIdOrSn -> YuntanT m Json
getOrder = dataFetch <<< GetOrder

removeOrder
  :: forall m. MonadAff m => MonadThrow Error m
  => OrderIdOrSn -> YuntanT m Unit
removeOrder = dataFetch <<< RemoveOrder
