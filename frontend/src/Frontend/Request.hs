module Frontend.Request where

import Control.Monad.Trans.Class
import Obelisk.Route.Frontend
import Reflex

import Matrix.Client.Types

data FrontendRequest a where
  FrontendRequest_Login :: LoginResponse -> FrontendRequest ()

class MonadFrontendRequest t m | m -> t where
  performFrontendRequest :: Event t (FrontendRequest a) -> m (Event t a)
  performFrontendRequest_ :: Event t (FrontendRequest a) -> m ()

instance (Monad m, MonadFrontendRequest t m) => MonadFrontendRequest t (RoutedT t r m) where
  performFrontendRequest = lift . performFrontendRequest
  performFrontendRequest_ = lift . performFrontendRequest_
