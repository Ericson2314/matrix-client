{-# LANGUAGE UndecidableInstances #-}
module Frontend.Request.Local where

import           Control.Concurrent
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
import           Data.Text (Text)
import           Data.Text.Encoding
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
import           Frontend.Request
import           Frontend.Schema

data XhrResponseParse response error
  = XhrResponseParse_XhrException XhrException
  | XhrResponseParse_Success response
  | XhrResponseParse_Failure error
  | XhrResponseParse_NoParseSuccess
  | XhrResponseParse_NoParseFailure
  deriving (Eq, Ord, Show)

data LocalFrontendRequestContext t = LocalFrontendRequestContext
  { _localFrontendRequestContext_connection :: Maybe (MVar Sqlite.Connection)
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
  performFrontendRequest req = performEventAsync $ ffor req $ \r k ->
    ReaderT $ \c -> handleLocalFrontendRequest @js @t @m k c r
  performFrontendRequest_ req = performEvent_ $ ffor req $ \r ->
    ReaderT $ \c -> handleLocalFrontendRequest @js @t @m (const $ return ()) c r

handleLocalFrontendRequest
  :: forall js t m a. (PerformEvent t m, Prerender js m)
  => (a -> IO ())
  -> LocalFrontendRequestContext t
  -> FrontendRequest a
  -> Performable m ()
handleLocalFrontendRequest k c = \case
  FrontendRequest_Login hs u pw -> do
    let loginRequest = LoginRequest
          (UserIdentifier_User u)
          (Login_Password pw)
          Nothing
          Nothing
    let url = hs <> "/_matrix/client/r0/login"
    performXhrCallbackWithErrorPrerenderJSON @js @t @m "POST" url loginRequest $ \case
      XhrResponseParse_Success r -> do
        void $ withConnection c $ \conn -> runBeamSqlite conn $ do
          -- TODO: Add upsert support for beam-sqlite.
          old <- runSelectReturningOne $ lookup_ (dbLogin db) $
            EntityKey $ Id $ UserId u
          -- TODO: Add entity_value lens to obelisk-beam.
          runUpdate $ update (dbLogin db)
            (\login -> (_login_isActive $ _entity_value login) <-. val_ False)
            (\_ -> val_ True)
          let new = Entity (Id $ UserId u) $ Login
                { _login_homeServer = hs
                , _login_accessToken = Just $ _loginResponse_accessToken r
                , _login_deviceId = Just $ _loginResponse_deviceId r
                , _login_isActive = True
                }
          case old of
            Nothing -> runInsert $ insert (dbLogin db) $ insertValues [new]
            Just _ -> runUpdate $ save (dbLogin db) new
        k ()
      XhrResponseParse_Failure (r :: Data.Aeson.Value) -> do
        print r
        k ()
      r -> do
        print r
        k ()

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

performXhrCallbackWithErrorPrerenderJSON
  :: forall js t m request response error
  .  (PerformEvent t m, Prerender js m, ToJSON request, FromJSON response, FromJSON error)
  => Text
  -> Text
  -> request
  -> (XhrResponseParse response error -> IO ())
  -> Performable m ()
performXhrCallbackWithErrorPrerenderJSON method url request k =
  performRequestCallbackWithErrorPrerender @js @t @m k' $
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

-- | No-op on prerender.
performRequestCallbackWithErrorPrerender
  :: forall js t m a. (PerformEvent t m, Prerender js m, IsXhrPayload a)
  => (Either XhrException XhrResponse -> IO ())
  -> XhrRequest a
  -> Performable m ()
performRequestCallbackWithErrorPrerender k req =
  prerenderPerformable @js @m (return ()) $
    performRequestCallbackWithError @(Performable m) @a k req

runLocalFrontendRequestT
  :: (Monad m, Prerender js m)
  => LocalFrontendRequestT t m a
  -> m a
runLocalFrontendRequestT (LocalFrontendRequestT m) = do
  -- TODO: Find some way to close the DB connection when the app is exited.
  conn <- prerender (return Nothing) $ liftIO $ fmap Just $ newMVar =<< initDb
  runReaderT m $ LocalFrontendRequestContext
    { _localFrontendRequestContext_connection = conn
    }
