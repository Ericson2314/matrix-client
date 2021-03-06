module Frontend.Request where

import Control.Monad.Trans.Class
import Data.Aeson
import Data.Aeson.Utils
import Data.DependentXhr
import Data.Kind
import Data.Text (Text)
import Data.Word
import Obelisk.Route.Frontend
import Reflex
import Reflex.Dom.Core (XhrException)

import Matrix.Client.Types as M hiding (Event)
import Matrix.Identifiers as M

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
    -> FrontendRequest (Either (FrontendError Value) AccessToken)
    -- ^ TODO be better than `Value` here
    -- ^ TODO change 'AccessToken' back to '()' once views are implemented.
  FrontendRequest_ListPublicRooms
    :: ServerName -- ^ home server
    -> Word32 -- ^ limit
    -> Maybe Text -- ^ since token
    -> FrontendRequest (Either (FrontendError Value) -- TODO be better than `Value` here
                               PublicRoomsResponse)
  FrontendRequest_JoinRoom
    :: ServerName -- ^ home server
    -> AccessToken
    -> RoomId -- ^ The room
    -> FrontendRequest (Either (FrontendError Value) ()) -- TODO be better than `Value` here

class MonadFrontendRequest t m | m -> t where
  performFrontendRequest
    :: Event t (FrontendRequest a) -> m (Event t a)
  performFrontendRequest_
    :: Event t (FrontendRequest a) -> m ()

instance (Monad m, MonadFrontendRequest t m) => MonadFrontendRequest t (RoutedT t r m) where
  performFrontendRequest = lift . performFrontendRequest
  performFrontendRequest_ = lift . performFrontendRequest_
