{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecursiveDo #-}
module Frontend.Request.Local where

import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Exception
import           Control.Monad.Primitive
import           Control.Monad.Ref
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.ByteString.Lazy (toStrict)
import           Data.Coerce
import           Data.Constraint
import qualified Data.Map.Monoidal as MM
import           Data.Semigroup
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import           Data.Vessel
import           Database.Beam
import           Database.Beam.Sqlite
import qualified Database.SQLite.Simple as Sqlite
import           Language.Javascript.JSaddle.Types
import           Obelisk.Database.Beam.Entity
import           Obelisk.Route.Frontend
import           Reflex.Dom.Core hiding (select)
import           Reflex.Dom.Prerender.Performable
import           Reflex.Host.Class

import           Matrix.Client.Types

import           Frontend.DB
import           Frontend.Query
import           Frontend.Request
import           Frontend.Schema

data XhrResponseParse response error
  = XhrResponseParse_XhrException XhrException
  | XhrResponseParse_Success response
  | XhrResponseParse_Failure error
  | XhrResponseParse_NoParseSuccess
  | XhrResponseParse_NoParseFailure
  deriving (Eq, Ord, Show)

data LocalFrontendRequestContext (t :: *) = LocalFrontendRequestContext
  { _localFrontendRequestContext_connection :: Maybe (MVar Sqlite.Connection)
  , _localFrontendRequestContext_updateQueryResult :: FrontendV Identity -> IO ()
  }

newtype LocalFrontendRequestT t m a = LocalFrontendRequestT
  { unLocalFrontendRequestT :: ReaderT (LocalFrontendRequestContext t) m a
  } deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadTrans
    , MonadFix
    , MonadIO
    , MonadRef
    , MonadException
    , MonadHold t
    , MonadSample t
    , TriggerEvent t
    , MonadReflexCreateTrigger t
    , PostBuild t
    , PerformEvent t
    , NotReady t
    , DomBuilder t
    , HasDocument
    , MonadJSM
    , HasJS js
    , HasJSContext
    , MonadQuery t q
    )

-- It looks like a GHC bug prevents this from being derived.
instance (MonadHold t m, MonadFix m, Adjustable t m) => Adjustable t (LocalFrontendRequestT t m) where
  runWithReplace a0 a' = LocalFrontendRequestT $ runWithReplace (coerce a0) (coerceEvent a')
  traverseDMapWithKeyWithAdjust f dm0 dm' = LocalFrontendRequestT $ traverseDMapWithKeyWithAdjust (\k v -> unLocalFrontendRequestT $ f k v) (coerce dm0) (coerceEvent dm')
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = LocalFrontendRequestT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> unLocalFrontendRequestT $ f k v) (coerce dm0) (coerceEvent dm')
  traverseIntMapWithKeyWithAdjust f dm0 dm' = LocalFrontendRequestT $ traverseIntMapWithKeyWithAdjust (coerce . f) dm0 dm'

-- It looks like a GHC bug prevents this from being derived.
instance Prerender js m => Prerender js (LocalFrontendRequestT t m) where
  prerenderClientDict = fmap (\Dict -> Dict) (prerenderClientDict :: Maybe (Dict (PrerenderClientConstraint js m)))

-- It looks like a GHC bug prevents this from being derived.
instance (Monad m, SetRoute t r m) => SetRoute t r (LocalFrontendRequestT t m) where
  setRoute = lift . setRoute
  modifyRoute = lift . modifyRoute

-- It looks like a GHC bug prevents this from being derived.
instance (Monad m, RouteToUrl r m) => RouteToUrl r (LocalFrontendRequestT t m) where
  askRouteToUrl = lift $ askRouteToUrl

instance PrimMonad m => PrimMonad (LocalFrontendRequestT t m) where
  type PrimState (LocalFrontendRequestT t m) = PrimState m
  primitive = lift . primitive

instance (Reflex t, PerformEvent t m, TriggerEvent t m, Prerender js m) => MonadFrontendRequest t (LocalFrontendRequestT t m) where
  performFrontendRequest req = performEventAsync $ ffor req $ \r k -> ReaderT $ \c ->
    prerenderPerformable @js @m blank $
      handleLocalFrontendRequest k c r
  performFrontendRequest_ req = performEvent_ $ ffor req $ \r -> ReaderT $ \c ->
    prerenderPerformable @js @m blank $
      handleLocalFrontendRequest (const blank) c r

handleLocalFrontendRequest
  :: JSConstraints js m
  => (a -> IO ())
  -> LocalFrontendRequestContext t
  -> FrontendRequest a
  -> m ()
handleLocalFrontendRequest k c = \case
  FrontendRequest_Login hs u pw -> do
    let loginRequest = LoginRequest
          (UserIdentifier_User u)
          (Login_Password pw)
          Nothing
          Nothing
    let url = hs <> "/_matrix/client/r0/login"
    performXhrCallbackWithErrorJSON "POST" url loginRequest $ \case
      XhrResponseParse_Success r -> do
        let uid = Id $ UserId u
            newValue = Login
              { _login_homeServer = hs
              , _login_accessToken = Just $ r ^. loginResponse_accessToken
              , _login_deviceId = Just $ r ^. loginResponse_deviceId
              , _login_isActive = True
              }
            newEntity = Entity uid newValue
        void $ withConnection c $ \conn -> runBeamSqlite conn $ do
          -- TODO: Add upsert support for beam-sqlite.
          old <- runSelectReturningOne $ lookup_ (dbLogin db) $ EntityKey uid
          runUpdate $ update (dbLogin db)
            (\login -> (login ^. entity_value . login_isActive) <-. val_ False)
            (\_ -> val_ True)
          case old of
            Nothing -> runInsert $ insert (dbLogin db) $ insertValues [newEntity]
            Just _ -> runUpdate $ save (dbLogin db) newEntity
        -- TODO: Add to V_Logins as well.
        patchQueryResult c $ singletonV V_Login $ MapV $
          MM.singleton uid $ Identity $ First $ Just newValue
        k $ Right ()
      XhrResponseParse_Failure e -> k $ Left $ FrontendError_ResponseError e
      r -> k $ Left $ FrontendError_Other $ T.pack $ show r

patchQueryResult :: LocalFrontendRequestContext t -> FrontendV Identity -> IO ()
patchQueryResult c = _localFrontendRequestContext_updateQueryResult c

withConnection
  :: MonadIO m
  => LocalFrontendRequestContext t
  -> (Sqlite.Connection -> m a)
  -> m (Maybe a)
withConnection c k = forM (_localFrontendRequestContext_connection c) $
  k <=< liftIO . readMVar

withConnectionTransaction
  :: MonadIO m
  => LocalFrontendRequestContext t
  -> (Sqlite.Connection -> IO a)
  -> m (Maybe a)
withConnectionTransaction c k =
  forM (_localFrontendRequestContext_connection c) $ \mvc -> liftIO $ do
    conn <- readMVar mvc
    Sqlite.withTransaction conn $ k conn

performXhrCallbackWithErrorJSON
  :: forall js m request response error
  .  (JSConstraints js m, ToJSON request, FromJSON response, FromJSON error)
  => Text
  -> Text
  -> request
  -> (XhrResponseParse response error -> IO ())
  -> m ()
performXhrCallbackWithErrorJSON method url request k =
  performRequestCallbackWithError k' $
    XhrRequest method url $ def & xhrRequestConfig_sendData .~ body
 where
  body = decodeUtf8 $ toStrict $ Data.Aeson.encode request
  k' = \case
    Left e -> k $ XhrResponseParse_XhrException e
    Right r -> if _xhrResponse_status r >= 200 && _xhrResponse_status r < 400
      then k $ maybe XhrResponseParse_NoParseSuccess XhrResponseParse_Success $ decodeXhrResponse r
      else k $ maybe XhrResponseParse_NoParseFailure XhrResponseParse_Failure $ decodeXhrResponse r

performRequestCallbackWithError
  :: forall m a. (MonadJSM m, HasJSContext m, IsXhrPayload a)
  => (Either XhrException XhrResponse -> IO ())
  -> XhrRequest a
  -> m ()
performRequestCallbackWithError k req =
  void $ newXMLHttpRequestWithError req $ liftIO . k

runLocalFrontendRequestT
  :: (Reflex t, MonadFix m, MonadHold t m, TriggerEvent t m, MonadIO m, Prerender js m)
  => LocalFrontendRequestT t (QueryT t (FrontendV (Const SelectedCount)) m) a
  -> m a
runLocalFrontendRequestT (LocalFrontendRequestT m) = do
  -- TODO: Find some way to close the DB connection when the app is exited.
  conn <- prerender (return Nothing) $ liftIO $ fmap Just $ newMVar =<< initDb
  (queryResultPatch, updateQueryResult) <- newTriggerEvent
  rec queryResult <- cropDyn (incrementalToDynamic q) queryResultPatch
      (a, q) <- flip runQueryT queryResult $ runReaderT m $ LocalFrontendRequestContext
        { _localFrontendRequestContext_connection = conn
        , _localFrontendRequestContext_updateQueryResult = updateQueryResult
        }
  return a

-- TODO: Expose this in reflex (currently it's hidden in reflex-dom).
-- Also is cropping here really necessary or will `runQueryT` do it?
cropDyn :: (Query q, MonadHold t m, Reflex t, MonadFix m) => Dynamic t q -> Event t (QueryResult q) -> m (Dynamic t (QueryResult q))
cropDyn q = foldDyn (\(q', qr) v -> crop q' (qr `mappend` v)) mempty . attach (current q)
