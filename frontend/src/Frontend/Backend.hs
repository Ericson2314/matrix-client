{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeInType #-}
module Frontend.Backend where

import           Control.Concurrent
import           Control.Lens hiding (has)
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Exception
import           Control.Monad.Logger
import           Control.Monad.Morph
import           Control.Monad.Primitive
import           Control.Monad.Ref
import           Control.Monad.Trans.Reader
import           Data.Coerce
import           Data.Constraint
import qualified Database.SQLite.Simple as Sqlite
import           Language.Javascript.JSaddle.Types
import           Obelisk.Route.Frontend
import           Reflex.Dom.Core hiding (select)
import           Reflex.Dom.Prerender.Performable
import           Reflex.Host.Class
import           Reflex.Orphans ()

import           Frontend.DB
import           Frontend.Query
import           Frontend.Request
import           Frontend.Backend.Common
import           Frontend.Backend.RequestHandler
import           Frontend.Backend.QueryHandler


data FrontendBackendContext = FrontendBackendContext
  { _localFrontendRequestContext_connection :: Maybe (MVar Sqlite.Connection)
  , _localFrontendRequestContext_updateQueryResult :: FrontendV Identity -> IO ()
  , _localFrontendRequestContext_logger :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
  }

makeLenses ''FrontendBackendContext

instance GivesSQLiteConnection FrontendBackendContext where
  obtainSqliteConnection = obtainSqliteConnection
    . view localFrontendRequestContext_connection

instance HasQueryCallback FrontendBackendContext where
  updateQueryResult = localFrontendRequestContext_updateQueryResult

instance HasLogger FrontendBackendContext where
  logger = localFrontendRequestContext_logger

newtype FrontendBackendT m a = FrontendBackendT
  { unFrontendBackendT :: ReaderT FrontendBackendContext m a
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
instance (MonadHold t m, MonadFix m, Adjustable t m) => Adjustable t (FrontendBackendT m) where
  runWithReplace a0 a' = FrontendBackendT $ runWithReplace (coerce a0) (coerceEvent a')
  traverseDMapWithKeyWithAdjust f dm0 dm' = FrontendBackendT $ traverseDMapWithKeyWithAdjust (\k v -> unFrontendBackendT $ f k v) (coerce dm0) (coerceEvent dm')
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = FrontendBackendT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> unFrontendBackendT $ f k v) (coerce dm0) (coerceEvent dm')
  traverseIntMapWithKeyWithAdjust f dm0 dm' = FrontendBackendT $ traverseIntMapWithKeyWithAdjust (coerce . f) dm0 dm'

-- It looks like a GHC bug prevents this from being derived.
instance Prerender js m => Prerender js (FrontendBackendT m) where
  prerenderClientDict = fmap (\Dict -> Dict) (prerenderClientDict :: Maybe (Dict (PrerenderClientConstraint js m)))

-- It looks like a GHC bug prevents this from being derived.
instance (Monad m, SetRoute t r m) => SetRoute t r (FrontendBackendT m) where
  setRoute = lift . setRoute
  modifyRoute = lift . modifyRoute

-- It looks like a GHC bug prevents this from being derived.
instance (Monad m, RouteToUrl r m) => RouteToUrl r (FrontendBackendT m) where
  askRouteToUrl = lift $ askRouteToUrl

instance PrimMonad m => PrimMonad (FrontendBackendT m) where
  type PrimState (FrontendBackendT m) = PrimState m
  primitive = lift . primitive

instance MonadIO m => MonadLogger (FrontendBackendT m) where
  monadLoggerLog a b c msg = FrontendBackendT $ do
    logger' <- asks _localFrontendRequestContext_logger
    liftIO $ logger' a b c $ toLogStr msg

instance MonadIO m => MonadLoggerIO (FrontendBackendT m) where
  askLoggerIO = FrontendBackendT $ asks _localFrontendRequestContext_logger

-- TODO use `coerce`
instance PerformEvent t m => PerformEvent t (FrontendBackendT m) where
  type Performable (FrontendBackendT m) = FrontendBackendT (Performable m)
  performEvent_ = FrontendBackendT . performEvent_ . fmap unFrontendBackendT
  performEvent = FrontendBackendT . performEvent . fmap unFrontendBackendT

instance
  (Reflex t, PerformEvent t m, TriggerEvent t m, Prerender js m)
  => MonadFrontendRequest t (FrontendBackendT m)
 where
  performFrontendRequest req = FrontendBackendT $ do
    performEventAsync $ ffor req $ \r k ->
      prerenderPerformable @js @(ReaderT _ m) blank $
        handleLocalFrontendRequest r $ liftIO . k
  performFrontendRequest_ req = FrontendBackendT $ do
    performEvent_ $ ffor req $ \r ->
      prerenderPerformable @js @(ReaderT _ m) blank $
        -- flip runReaderT ctx $ handleLocalFrontendRequest r $ const blank
        handleLocalFrontendRequest r $ const blank

runFrontendBackendT
  :: forall m t js a
  .  ( MonadFix m
     , MonadHold t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadIO m
     , Prerender js m
     , PostBuild t m
     )
  => FrontendBackendT (QueryT t (FrontendV (Const SelectedCount)) m) a
  -> m a
runFrontendBackendT (FrontendBackendT m) = do
  -- TODO: Find some way to close the DB connection when the app is exited.
  conn <- prerender (return Nothing) $ liftIO $ fmap Just $ newMVar =<< initDb
  -- TODO monad-logger should give us the underlying implementation whether or
  -- not we in a `MonadIO`.
  logger' :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
    <- prerender (return $ \ _ _ _ _ -> blank @IO) $ liftIO $
      (runStderrLoggingT $ askLoggerIO :: IO (Loc -> LogSource -> LogLevel -> LogStr -> IO ()))
  flip runReaderT conn $ runFrontendQueries @(ReaderT _ m) $ \updateQueryResult' ->
    (hoist (ReaderT . const)) $ runReaderT m $ FrontendBackendContext
      { _localFrontendRequestContext_connection = conn
      , _localFrontendRequestContext_updateQueryResult = updateQueryResult'
      , _localFrontendRequestContext_logger = logger'
      }
