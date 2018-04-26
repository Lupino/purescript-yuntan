module Yuntan.Utils
  ( ServiceFunc
  , importFn0
  , importFn1
  , importFn2
  , importFn3
  ) where

import Prelude
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Promise (Promise, toAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Yuntan.Trans (Service, ServiceT)
import Control.Monad.Reader.Class (ask)

type ServiceFunc = String

foreign import _importFn0
  :: forall eff a. ServiceFunc -> Service -> Eff eff (Promise a)

foreign import _importFn1
  :: forall eff a arg1. ServiceFunc -> arg1 -> Service -> Eff eff (Promise a)

foreign import _importFn2
  :: forall eff a arg1 arg2. ServiceFunc -> arg1 -> arg2 -> Service -> Eff eff (Promise a)

foreign import _importFn3
  :: forall eff a arg1 arg2 arg3. ServiceFunc -> arg1 -> arg2 -> arg3 -> Service -> Eff eff (Promise a)

toServiceT
  :: forall eff m a. MonadAff eff m => MonadEff eff m
  => (Service -> Eff eff (Promise a)) -> ServiceT m a
toServiceT f = do
  s <- ask
  p <- liftEff $ f s
  liftAff $ toAff p

importFn0
  :: forall eff m a. MonadAff eff m => MonadEff eff m
  => ServiceFunc -> ServiceT m a
importFn0 f = toServiceT (_importFn0 f)

importFn1
  :: forall eff m a arg1. MonadAff eff m => MonadEff eff m
  => ServiceFunc -> arg1 -> ServiceT m a
importFn1 f arg1 = toServiceT (_importFn1 f arg1)

importFn2
  :: forall eff m a arg1 arg2. MonadAff eff m => MonadEff eff m
  => ServiceFunc -> arg1 -> arg2 -> ServiceT m a
importFn2 f arg1 arg2 = toServiceT (_importFn2 f arg1 arg2)

importFn3
  :: forall eff m a arg1 arg2 arg3. MonadAff eff m => MonadEff eff m
  => ServiceFunc -> arg1 -> arg2 -> arg3 -> ServiceT m a
importFn3 f arg1 arg2 arg3 = toServiceT (_importFn3 f arg1 arg2 arg3)
