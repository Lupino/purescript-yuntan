module Yuntan.Internal.Utils
  ( ServiceFunc
  , fromFn0
  , fromFn1
  , fromFn2
  , fromFn3
  , fromAFn0
  , fromAFn1
  , fromAFn2
  , fromAFn3
  ) where

import Prelude
import Effect.Aff.Class (class MonadAff, liftAff)
import Control.Promise (Promise, toAff)
import Effect (Effect)
import Effect.Class (liftEffect)
import Yuntan.Internal.Trans (Service, ServiceT)
import Control.Monad.Reader.Class (ask)
import Effect.Aff.Compat (fromEffectFnAff, EffectFnAff)

type ServiceFunc = String

foreign import _fromFn0
  :: forall a. ServiceFunc -> Service -> Effect (Promise a)

foreign import _fromFn1
  :: forall a arg1. ServiceFunc -> arg1 -> Service -> Effect (Promise a)

foreign import _fromFn2
  :: forall a arg1 arg2. ServiceFunc -> arg1 -> arg2 -> Service -> Effect (Promise a)

foreign import _fromFn3
  :: forall a arg1 arg2 arg3. ServiceFunc -> arg1 -> arg2 -> arg3 -> Service -> Effect (Promise a)

mkServT
  :: forall m a. MonadAff m => (Service -> Effect (Promise a)) -> ServiceT m a
mkServT f = do
  s <- ask
  p <- liftEffect $ f s
  liftAff $ toAff p

fromFn0
  :: forall m a. MonadAff m
  => ServiceFunc -> ServiceT m a
fromFn0 f = mkServT (_fromFn0 f)

fromFn1
  :: forall m a arg1. MonadAff m
  => ServiceFunc -> arg1 -> ServiceT m a
fromFn1 f arg1 = mkServT (_fromFn1 f arg1)

fromFn2
  :: forall m a arg1 arg2. MonadAff m
  => ServiceFunc -> arg1 -> arg2 -> ServiceT m a
fromFn2 f arg1 arg2 = mkServT (_fromFn2 f arg1 arg2)

fromFn3
  :: forall m a arg1 arg2 arg3. MonadAff m
  => ServiceFunc -> arg1 -> arg2 -> arg3 -> ServiceT m a
fromFn3 f arg1 arg2 arg3 = mkServT (_fromFn3 f arg1 arg2 arg3)

foreign import _fromAFn0 :: forall a. ServiceFunc -> Service -> EffectFnAff a

foreign import _fromAFn1
  :: forall a arg1. ServiceFunc -> arg1 -> Service -> EffectFnAff a

foreign import _fromAFn2
  :: forall a arg1 arg2. ServiceFunc -> arg1 -> arg2 -> Service -> EffectFnAff a

foreign import _fromAFn3
  :: forall a arg1 arg2 arg3. ServiceFunc -> arg1 -> arg2 -> arg3 -> Service -> EffectFnAff a

mkServTA
  :: forall m a. MonadAff m => (Service -> EffectFnAff a) -> ServiceT m a
mkServTA f = liftAff <<< fromEffectFnAff <<< f =<< ask

fromAFn0 :: forall m a. MonadAff m => ServiceFunc -> ServiceT m a
fromAFn0 f = mkServTA (_fromAFn0 f)

fromAFn1 :: forall m a arg1. MonadAff m => ServiceFunc -> arg1 -> ServiceT m a
fromAFn1 f arg1 = mkServTA (_fromAFn1 f arg1)

fromAFn2
  :: forall m a arg1 arg2. MonadAff m => ServiceFunc -> arg1 -> arg2 -> ServiceT m a
fromAFn2 f arg1 arg2 = mkServTA (_fromAFn2 f arg1 arg2)

fromAFn3
  :: forall m a arg1 arg2 arg3. MonadAff m => ServiceFunc -> arg1 -> arg2 -> arg3 -> ServiceT m a
fromAFn3 f arg1 arg2 arg3 = mkServTA (_fromAFn3 f arg1 arg2 arg3)
