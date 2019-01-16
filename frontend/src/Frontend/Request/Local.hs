{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeInType #-}
module Frontend.Request.Local where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Lens hiding (has)
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Exception
import           Control.Monad.Logger
import           Control.Monad.Primitive
import           Control.Monad.Ref
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Coerce
import           Data.Constraint
import           Data.DependentXhr
import           Data.Kind
import qualified Data.Map.Monoidal as MM
import           Data.Maybe
import           Data.Semigroup
import qualified Data.Set as S
import           Data.Vessel
import           Database.Beam
import           Database.Beam.Sqlite
import qualified Database.SQLite.Simple as Sqlite
import           GHC.TypeLits
import           Language.Javascript.JSaddle.Types
import           Obelisk.Database.Beam.Entity
import           Obelisk.Route.Frontend
import           Reflex.Dom.Core hiding (select)
import           Reflex.Dom.Prerender.Performable
import           Reflex.Host.Class

import           Matrix.Client.Types as M hiding (Event)
import           Matrix.Identifiers as M

import           Frontend.DB
import           Frontend.Query
import           Frontend.Request
import           Frontend.Schema


data LocalFrontendRequestContext (t :: Type) = LocalFrontendRequestContext
  { _localFrontendRequestContext_connection :: Maybe (MVar Sqlite.Connection)
  , _localFrontendRequestContext_currentQuery :: Behavior t (FrontendV (Const SelectedCount))
  , _localFrontendRequestContext_updateQueryResult :: FrontendV Identity -> IO ()
  , _localFrontendRequestContext_logger :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
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

instance MonadIO m => MonadLogger (LocalFrontendRequestT t m) where
  monadLoggerLog a b c msg = LocalFrontendRequestT $ do
    logger <- asks _localFrontendRequestContext_logger
    liftIO $ logger a b c $ toLogStr msg

instance MonadIO m => MonadLoggerIO (LocalFrontendRequestT t m) where
  askLoggerIO = LocalFrontendRequestT $ asks _localFrontendRequestContext_logger

-- TODO use `coerce`
instance PerformEvent t m => PerformEvent t (LocalFrontendRequestT t m) where
  type Performable (LocalFrontendRequestT t m) = LocalFrontendRequestT t (Performable m)
  performEvent_ = LocalFrontendRequestT . performEvent_ . fmap unLocalFrontendRequestT
  performEvent = LocalFrontendRequestT . performEvent . fmap unLocalFrontendRequestT

instance (Reflex t, PerformEvent t m, TriggerEvent t m, Prerender js m) => MonadFrontendRequest t (LocalFrontendRequestT t m) where
  performFrontendRequest req = do
    ctx <- LocalFrontendRequestT ask
    performEventAsync $ ffor req $ \r k ->
      prerenderPerformable @js @(LocalFrontendRequestT t m) blank $
        handleLocalFrontendRequest (liftIO . k) ctx r
  performFrontendRequest_ req = do
    ctx <- LocalFrontendRequestT ask
    performEvent_ $ ffor req $ \r ->
      prerenderPerformable @js @(LocalFrontendRequestT t m) blank $
        handleLocalFrontendRequest (const blank) ctx r

-- | Converts an 'XhrResponseParse' to a 'FrontendError'. All the failure cases
-- are handled in the simplest way possible (pure functional boilerplate), while
-- the success case is handled via the function parameter.
convertErrors
  :: Monad m
  => (forall status r
      .  KnownNat status
      => (respPerCode :: RespRelation) status r
      -> r
      -> m (Either (FrontendError e) b))
  -> XhrResponseParse respPerCode
  -> m (Either (FrontendError e) b)
convertErrors handleSuccessful = \case
  Left xhrException ->
    pure $ Left $ FrontendError_Other $ Left xhrException
  Right (Left innvalidStatus) ->
    pure $ Left $ FrontendError_Other $ Right $ Left innvalidStatus
  Right (Right (XhrThisStatus _ (Left eNoBody))) ->
    pure $ Left $ FrontendError_Other $ Right $ Right $ Left eNoBody
  Right (Right (XhrThisStatus _ (Right (Left eBadJson)))) ->
    pure $ Left $ FrontendError_Other $ Right $ Right $ Right eBadJson
  Right (Right (XhrThisStatus sentinel (Right (Right r)))) -> handleSuccessful sentinel r

handleLocalFrontendRequest
  :: forall js m t a
  .  JSConstraints js m
  => (a -> LoggingT IO ())
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
    performRoutedRequest (ClientServerRoute_Login LoginRoute_Login) hs loginRequest QPList_Nil $ cvtE $ \sentinel r -> case sentinel of
      LoginRespKey_400 -> pure $ Left $ FrontendError_ResponseError r
      LoginRespKey_403 -> pure $ Left $ FrontendError_ResponseError r
      LoginRespKey_429 -> pure $ Left $ FrontendError_ResponseError r
      LoginRespKey_200 -> do
        let uid = r ^. loginResponse_userId
            uid' = Id $ printUserId uid
            newValue = Login
              { _login_homeServer = hs
              , _login_accessToken = Just $ r ^. loginResponse_accessToken
              , _login_deviceId = Just $ r ^. loginResponse_deviceId
              , _login_isActive = True
              }
            newEntity = Entity uid' newValue
        lids <- liftIO $ withConnection c $ \conn -> runBeamSqlite conn $ do
          -- TODO: Add upsert support for beam-sqlite.
          old <- runSelectReturningOne $ lookup_ (dbLogin db) $ EntityKey uid'
          runUpdate $ update (dbLogin db)
            (\login -> (login ^. entity_value . login_isActive) <-. val_ False)
            (\_ -> val_ True)
          case old of
            Nothing -> runInsert $ insert (dbLogin db) $ insertValues [newEntity]
            Just _ -> runUpdate $ save (dbLogin db) newEntity
          runSelectReturningList $ select $ pk <$> all_ (dbLogin db)
        $(logInfo) $ "Sucessfully logged in user: " <> printUserId uid
        lift $ patchQueryResult c $ mconcat
          [ singletonV V_Login $ MapV $
              MM.singleton (EntityKey uid') $ Identity $ First $ Just newValue
          -- TODO: Add accessor for value inside `EntityKey`.
          , singletonV V_Logins $ SingleV $ Identity $ First $
              S.fromList <$> lids
          ]
        pure $ Right $ r ^. loginResponse_accessToken
  FrontendRequest_JoinRoom hs token room -> do
    performRoutedRequest (ClientServerRoute_Room RoomRoute_Join) hs token (JoinRequest Nothing) room QPList_Nil $ cvtE $ \sentinal r -> case sentinal of
      JoinRespKey_403 -> pure $ Left $ FrontendError_ResponseError r
      JoinRespKey_429 -> pure $ Left $ FrontendError_ResponseError r
      JoinRespKey_200 -> do
        $(logInfo) $ "Sucessfully join room: " <> printRoomId room
        pure $ Right ()
  where
    logger = _localFrontendRequestContext_logger c
    -- | Arbitrary combinator soup. 'convertErrors' does the bulk of the work; the
    -- rest here is just whatever happens to be the common pattern for
    -- `handleLocalFrontendRequest`.
    cvtE
      :: forall respPerCode e b.  a ~ Either (FrontendError e) b
      => (forall s r. KnownNat s => respPerCode s r -> r -> LoggingT IO (Either (FrontendError e) b))
      -> XhrResponseParse respPerCode
      -> IO ()
    cvtE k' = flip runLoggingT logger . (k <=< convertErrors k')

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
  :: (MonadIO m, Reflex t)
  => LocalFrontendRequestContext t
  -> (Sqlite.Connection -> IO a)
  -> m (Maybe a)
withConnectionTransaction c k =
  forM (_localFrontendRequestContext_connection c) $ \mvc -> liftIO $ do
    conn <- readMVar mvc
    Sqlite.withTransaction conn $ k conn

handleQueryUpdates
  :: Reflex t
  => LocalFrontendRequestContext t
  -> TChan (FrontendV (Const SelectedCount))
  -> IO ()
handleQueryUpdates context queryPatchChan = do
  forever $ do
    patch <- readTChanConcat queryPatchChan
    -- TODO: Crop out negative selected counts in patch.
    void $ traverseWithKeyV (handleV context) patch
    return ()

handleV
  :: forall t f. Reflex t
  => LocalFrontendRequestContext t
  -> V f
  -> f (Const SelectedCount)
  -> IO (f Identity)
handleV context k v = case k of
  V_Login ->
    fmap (fromMaybe mempty) $ withConnection context $ \conn ->
      runBeamSqlite conn $ do
        logins <- runSelectReturningList $ select $ pk <$> all_ (dbLogin db)
        _ -- TODO: Traverse logins.

readTChanConcat :: Semigroup a => TChan a -> IO a
readTChanConcat c = atomically (readTChan c) >>= concatWaiting
  where
    concatWaiting b = atomically (tryReadTChan c) >>= \case
      Just a -> concatWaiting $ a <> b
      Nothing -> return b

runLocalFrontendRequestT
  :: ( Reflex t
     , MonadFix m
     , MonadHold t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadIO m
     , Prerender js m
     )
  => LocalFrontendRequestT t (QueryT t (FrontendV (Const SelectedCount)) m) a
  -> m a
runLocalFrontendRequestT (LocalFrontendRequestT m) = do
  -- TODO: Find some way to close the DB connection when the app is exited.
  conn <- prerender (return Nothing) $ liftIO $ fmap Just $ newMVar =<< initDb
  -- TODO monad-logger should give us the underlying implementation whether or
  -- not we in a `MonadIO`.
  logger :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
    <- prerender (return $ \ _ _ _ _ -> blank @IO) $ liftIO $
      (runStderrLoggingT $ askLoggerIO :: IO (Loc -> LogSource -> LogLevel -> LogStr -> IO ()))
  (queryResultPatch, updateQueryResult) <- newTriggerEvent
  rec let dq = incrementalToDynamic q
          queryPatch = updatedIncremental q
          context = LocalFrontendRequestContext
            { _localFrontendRequestContext_connection = conn
            , _localFrontendRequestContext_currentQuery = current dq
            , _localFrontendRequestContext_updateQueryResult = updateQueryResult
            , _localFrontendRequestContext_logger = logger
            }
      queryResult <- cropDyn dq queryResultPatch
      (a, q) <- flip runQueryT queryResult $ runReaderT m context
  prerender blank $ do
    queryPatchChan <- liftIO newTChanIO
    performEvent_ $ ffor queryPatch $ \qp -> liftIO $
      atomically $ writeTChan queryPatchChan $ unAdditivePatch qp
    liftIO $ void $ forkIO $ handleQueryUpdates context queryPatchChan
  return a

-- TODO: Expose this in reflex (currently it's hidden in reflex-dom).
-- Also is cropping here really necessary or will `runQueryT` do it?
cropDyn :: (Query q, MonadHold t m, Reflex t, MonadFix m) => Dynamic t q -> Event t (QueryResult q) -> m (Dynamic t (QueryResult q))
cropDyn q = foldDyn (\(q', qr) v -> crop q' (qr `mappend` v)) mempty . attach (current q)
