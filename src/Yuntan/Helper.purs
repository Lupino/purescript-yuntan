module Yuntan.Helper
  ( initService
  ) where

import Yuntan.Internal.Trans (ServiceName, ServiceType, Service)
import Effect (Effect)


foreign import initService :: forall opts. ServiceName -> ServiceType -> opts -> Effect Service
