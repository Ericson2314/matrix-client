module Frontend.Request where

import Control.Monad.Trans.Class
import Data.Aeson
import Data.Text (Text)
import Obelisk.Route.Frontend
import Reflex

-- TODO: Add matrix protocol's error type here.
data FrontendError
  = FrontendError_ResponseError Value
  | FrontendError_Other Text
  deriving (Eq, Show)

data FrontendRequest a where
  FrontendRequest_Login
    :: Text -- ^ home server
    -> Text -- ^ user name
    -> Text -- ^ password
    -> FrontendRequest (Either FrontendError ())

class MonadFrontendRequest t m | m -> t where
  performFrontendRequest :: Event t (FrontendRequest a) -> m (Event t a)
  performFrontendRequest_ :: Event t (FrontendRequest a) -> m ()

instance (Monad m, MonadFrontendRequest t m) => MonadFrontendRequest t (RoutedT t r m) where
  performFrontendRequest = lift . performFrontendRequest
  performFrontendRequest_ = lift . performFrontendRequest_
