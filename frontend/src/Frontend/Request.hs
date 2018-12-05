module Frontend.Request where

import Control.Monad.Trans.Class
import Data.Aeson
import Data.Aeson.Utils
import Data.DependentXhr
import Data.Kind
import Data.Text (Text)
import Obelisk.Route.Frontend
import Reflex
import Reflex.Dom.Core (XhrException)

-- | Instantiate with the possible errors of each call
data FrontendError e
  = FrontendError_ResponseError e
  | FrontendError_Other
    (Either XhrException
            (Either ErrorXhrInvalidStatus
                    (Either ErrorXhrNoBody
                            ErrorBadJsonParse)))
  deriving (Eq, Show)

data FrontendRequest :: Type -> Type where
  FrontendRequest_Login
    :: Text -- ^ home server
    -> Text -- ^ user name
    -> Text -- ^ password
    -> FrontendRequest (Either (FrontendError Value) ()) -- TODO be better than `Value` here

class MonadFrontendRequest t m | m -> t where
  performFrontendRequest
    :: Event t (FrontendRequest a) -> m (Event t a)
  performFrontendRequest_
    :: Event t (FrontendRequest a) -> m ()

instance (Monad m, MonadFrontendRequest t m) => MonadFrontendRequest t (RoutedT t r m) where
  performFrontendRequest = lift . performFrontendRequest
  performFrontendRequest_ = lift . performFrontendRequest_
