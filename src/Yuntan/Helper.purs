module Yuntan.Helper
  ( initService
  , ServiceType
  ) where

import Yuntan.Internal.Trans (ServiceName, Service)
import Effect (Effect)

type ServiceType = String


foreign import initService :: forall opts. ServiceName -> ServiceType -> opts -> Effect Service
