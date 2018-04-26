module Yuntan.Trans
  ( ServiceName
  , ServiceType
  , Service
  , serviceName
  , initService
  , ServiceT
  , YuntanT
  , runYuntanT
  , class DataSourceName
  , dataSourceName
  , class DataSource
  , fetch
  , dataFetch
  ) where

import Prelude
import Data.Foldable (find)
import Data.Maybe (Maybe (..))
import Data.Newtype (class Newtype)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (error, Error)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Error.Class (class MonadThrow, throwError, class MonadError, catchError)

type ServiceName = String
type ServiceType = String

foreign import data Service :: Type

foreign import initService :: forall opts eff. ServiceName -> ServiceType -> opts -> Eff eff Service

foreign import serviceName :: Service -> ServiceName

lookupService :: ServiceName -> Array Service -> Maybe Service
lookupService n = find isMatch
  where isMatch s = serviceName s == n

newtype ServiceT m a = ServiceT (ReaderT Service m a)

runServiceT :: forall m a. Service -> ServiceT m a -> m a
runServiceT s (ServiceT m)= runReaderT m s

derive instance newtypeServiceT :: Newtype (ServiceT m a) _

instance functorServiceT :: Functor m => Functor (ServiceT m) where
  map f (ServiceT m) = ServiceT $ map f m

instance applyServiceT :: Monad m => Apply (ServiceT m) where
  apply = ap

instance applicativeServiceT :: Monad m => Applicative (ServiceT m) where
  pure = ServiceT <<< pure

instance bindServiceT :: Monad m => Bind (ServiceT m) where
  bind (ServiceT m) k = ServiceT $ do
    a <- m
    case k a of
      ServiceT b -> b

instance monadServiceT :: Monad m => Monad (ServiceT m)

instance monadTransServiceT :: MonadTrans ServiceT where
  lift = ServiceT <<< lift

instance monadEffServiceT :: MonadEff eff m => MonadEff eff (ServiceT m) where
  liftEff = lift <<< liftEff

instance monadAffServiceT :: MonadAff eff m => MonadAff eff (ServiceT m) where
  liftAff = lift <<< liftAff

instance monadAskServiceT :: Monad m => MonadAsk Service (ServiceT m) where
  ask = ServiceT ask

instance monadThrowServiceT :: MonadThrow e m => MonadThrow e (ServiceT m) where
  throwError = ServiceT <<< throwError

instance monadErrorServiceT :: MonadError e m => MonadError e (ServiceT m) where
  catchError (ServiceT m) h =
    ServiceT $ catchError m (\e -> case h e of ServiceT f -> f)

newtype YuntanT m a = YuntanT (ReaderT (Array Service) m a)

runYuntanT :: forall m a. Array Service -> YuntanT m a -> m a
runYuntanT s (YuntanT m)= runReaderT m s

derive instance newtypeYuntanT :: Newtype (YuntanT m a) _

instance functorYuntanT :: Functor m => Functor (YuntanT m) where
  map f (YuntanT m) = YuntanT $ map f m

instance applyYuntanT :: Monad m => Apply (YuntanT m) where
  apply = ap

instance applicativeYuntanT :: Monad m => Applicative (YuntanT m) where
  pure = YuntanT <<< pure

instance bindYuntanT :: Monad m => Bind (YuntanT m) where
  bind (YuntanT m) k = YuntanT $ do
    a <- m
    case k a of
      YuntanT b -> b

instance monadYuntanT :: Monad m => Monad (YuntanT m)

instance monadTransYuntanT :: MonadTrans YuntanT where
  lift = YuntanT <<< lift

instance monadEffYuntanT :: MonadEff eff m => MonadEff eff (YuntanT m) where
  liftEff = lift <<< liftEff

instance monadAffYuntanT :: MonadAff eff m => MonadAff eff (YuntanT m) where
  liftAff = lift <<< liftAff

instance monadAskYuntanT :: Monad m => MonadAsk (Array Service) (YuntanT m) where
  ask = YuntanT ask

instance monadThrowYuntanT :: MonadThrow e m => MonadThrow e (YuntanT m) where
  throwError = YuntanT <<< throwError

instance monadErrorYuntanT :: MonadError e m => MonadError e (YuntanT m) where
  catchError (YuntanT m) h =
    YuntanT $ catchError m (\e -> case h e of YuntanT f -> f)

class DataSourceName req where
  dataSourceName :: req -> ServiceName

class Monad m <= DataSource m req where
  fetch :: forall a. req -> ServiceT m a

dataFetch
  :: forall m req a. Monad m => DataSource m req => DataSourceName req => MonadThrow Error m
  => req -> YuntanT m a
dataFetch req = do
  s' <- lookupService (dataSourceName req) <$> ask
  case s' of
    Nothing -> throwError $ error $ "Service " <> dataSourceName req <> " not found"
    Just s -> lift $ runServiceT s $ fetch req
