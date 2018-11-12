module Frontend.Request where

import Control.Monad.Trans.Class
import Data.Text (Text)
import Obelisk.Route.Frontend
import Reflex

data FrontendRequest a where
  FrontendRequest_Login
    :: Text -- ^ home server
    -> Text -- ^ user name
    -> Text -- ^ password
    -> FrontendRequest ()

class MonadFrontendRequest t m | m -> t where
  performFrontendRequest :: Event t (FrontendRequest a) -> m (Event t a)
  performFrontendRequest_ :: Event t (FrontendRequest a) -> m ()

instance (Monad m, MonadFrontendRequest t m) => MonadFrontendRequest t (RoutedT t r m) where
  performFrontendRequest = lift . performFrontendRequest
  performFrontendRequest_ = lift . performFrontendRequest_
