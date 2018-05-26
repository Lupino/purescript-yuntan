module Yuntan.Utils
  ( ServiceFunc
  , importFn0
  , importFn1
  , importFn2
  , importFn3
  ) where

import Prelude
import Effect.Aff.Class (class MonadAff, liftAff)
import Control.Promise (Promise, toAff)
import Effect (Effect)
import Effect.Class (liftEffect)
import Yuntan.Trans (Service, ServiceT)
import Control.Monad.Reader.Class (ask)

type ServiceFunc = String

foreign import _importFn0
  :: forall a. ServiceFunc -> Service -> Effect (Promise a)

foreign import _importFn1
  :: forall a arg1. ServiceFunc -> arg1 -> Service -> Effect (Promise a)

foreign import _importFn2
  :: forall a arg1 arg2. ServiceFunc -> arg1 -> arg2 -> Service -> Effect (Promise a)

foreign import _importFn3
  :: forall a arg1 arg2 arg3. ServiceFunc -> arg1 -> arg2 -> arg3 -> Service -> Effect (Promise a)

toServiceT
  :: forall m a. MonadAff m
  => (Service -> Effect (Promise a)) -> ServiceT m a
toServiceT f = do
  s <- ask
  p <- liftEffect $ f s
  liftAff $ toAff p

importFn0
  :: forall m a. MonadAff m
  => ServiceFunc -> ServiceT m a
importFn0 f = toServiceT (_importFn0 f)

importFn1
  :: forall m a arg1. MonadAff m
  => ServiceFunc -> arg1 -> ServiceT m a
importFn1 f arg1 = toServiceT (_importFn1 f arg1)

importFn2
  :: forall m a arg1 arg2. MonadAff m
  => ServiceFunc -> arg1 -> arg2 -> ServiceT m a
importFn2 f arg1 arg2 = toServiceT (_importFn2 f arg1 arg2)

importFn3
  :: forall m a arg1 arg2 arg3. MonadAff m
  => ServiceFunc -> arg1 -> arg2 -> arg3 -> ServiceT m a
importFn3 f arg1 arg2 arg3 = toServiceT (_importFn3 f arg1 arg2 arg3)
