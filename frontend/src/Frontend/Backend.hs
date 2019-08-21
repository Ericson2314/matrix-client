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
import           Data.Kind
import qualified Database.SQLite.Simple as Sqlite
import           Language.Javascript.JSaddle.Types
import           Obelisk.Configs
import           Obelisk.Frontend
import           Obelisk.Route.Frontend
import           Reflex.Dom.Core hiding (select)
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

newtype FrontendBackendT (t :: Type) m a = FrontendBackendT
  { unFrontendBackendT :: ReaderT (Dynamic t FrontendBackendContext) m a
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
    , Prerender js t
    , HasConfigs
    , HasCookies
    )

-- It looks like a GHC bug prevents this from being derived.
instance (MonadHold t m, MonadFix m, Adjustable t m) => Adjustable t (FrontendBackendT t m) where
  runWithReplace a0 a' = FrontendBackendT $ runWithReplace (coerce a0) (coerceEvent a')
  traverseDMapWithKeyWithAdjust f dm0 dm' = FrontendBackendT $ traverseDMapWithKeyWithAdjust (\k v -> unFrontendBackendT $ f k v) (coerce dm0) (coerceEvent dm')
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = FrontendBackendT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> unFrontendBackendT $ f k v) (coerce dm0) (coerceEvent dm')
  traverseIntMapWithKeyWithAdjust f dm0 dm' = FrontendBackendT $ traverseIntMapWithKeyWithAdjust (coerce . f) dm0 dm'

-- It looks like a GHC bug prevents this from being derived.
instance (Monad m, SetRoute t r m) => SetRoute t r (FrontendBackendT t m) where
  setRoute = lift . setRoute
  modifyRoute = lift . modifyRoute

-- It looks like a GHC bug prevents this from being derived.
instance (Monad m, RouteToUrl r m) => RouteToUrl r (FrontendBackendT t m) where
  askRouteToUrl = lift $ askRouteToUrl

instance PrimMonad m => PrimMonad (FrontendBackendT t m) where
  type PrimState (FrontendBackendT t m) = PrimState m
  primitive = lift . primitive

-- TODO use `coerce`
instance PerformEvent t m => PerformEvent t (FrontendBackendT t m) where
  type Performable (FrontendBackendT t m) = FrontendBackendT t (Performable m)
  performEvent_ = FrontendBackendT . performEvent_ . fmap unFrontendBackendT
  performEvent = FrontendBackendT . performEvent . fmap unFrontendBackendT

instance
  (Reflex t, PerformEvent t m, Prerender js t m)
  => MonadFrontendRequest t (FrontendBackendT t m)
 where
  performFrontendRequest req = FrontendBackendT $ do
    dctx <- ask
    fmap switchDyn $ prerender (return never) $
      performEventAsync $ ffor (attach (current dctx) req) $ \(ctx, r) k ->
        flip runReaderT ctx $ handleLocalFrontendRequest r $ liftIO . k
  performFrontendRequest_ req = FrontendBackendT $ do
    dctx <- ask
    prerender_ blank $
      performEvent_ $ ffor (attach (current dctx) req) $ \(ctx, r) ->
        flip runReaderT ctx $ handleLocalFrontendRequest r $ const blank

runFrontendBackendT
  :: forall m t js a
  .  ( MonadFix m
     , MonadHold t m
     , PerformEvent t m
     , TriggerEvent t m
     , Prerender js t m
     , PostBuild t m
     )
  => FrontendBackendT t (QueryT t (FrontendV (Const SelectedCount)) m) a
  -> m a
runFrontendBackendT (FrontendBackendT m) = do
  -- TODO: Find some way to close the DB connection when the app is exited.
  dmconn <- prerender (return Nothing) $ fmap Just $ liftIO $ newMVar =<< initDb
  runFrontendQueries dmconn $ \updateQueryResult' -> do
    let serverContext = FrontendBackendContext
          { _localFrontendRequestContext_connection = Nothing
          , _localFrontendRequestContext_updateQueryResult = updateQueryResult'
          , _localFrontendRequestContext_logger = return $ \ _ _ _ -> blank
          }
    dconnToContext <- prerender (return $ const serverContext) $ do
      -- TODO monad-logger should give us the underlying implementation whether or
      -- not we in a `MonadIO`.
      logger' <- liftIO $ runStderrLoggingT askLoggerIO
      return $ \mconn -> FrontendBackendContext
        { _localFrontendRequestContext_connection = mconn
        , _localFrontendRequestContext_updateQueryResult = updateQueryResult'
        , _localFrontendRequestContext_logger = logger'
        }
    runReaderT m $ dconnToContext <*> dmconn
