{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeInType #-}
module Frontend.Backend.QueryHandler where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Lens hiding (has)
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Logger
import qualified Data.Map as M
import qualified Data.Map.Monoidal as MM
import           Data.Maybe
import           Data.Proxy
import           Data.Semigroup
import qualified Data.Set as S
import           Data.Vessel
import           Database.Beam
import           Database.Beam.Keyed
import           Database.Beam.Sqlite
import           Language.Javascript.JSaddle.Types
import           Reflex.Dom.Core hiding (Key, select)
-- TODO move to reflex. This method doesn't use web stuff.
import           Reflex.Dom.WebSocket.Query (cropQueryT)
--import           Reflex.Dom.Prerender.Performable 

import           Matrix.Identifiers
import           Matrix.Client.Types hiding (Event, Login)

import           Data.DependentXhr
import           Frontend.Query
import           Frontend.Schema
import           Frontend.Backend.Common

handleQueryUpdates
  :: forall r
  .  GivesSQLiteConnection r
  => (FrontendV Identity -> IO ())
  -> TChan FrontendQuery'
  -> ReaderT r IO ()
handleQueryUpdates updateQueryResult' queryPatchChan = loop mempty
  where
    loop oldQuery = do
      newQuery <- liftIO $ readTChanConcat queryPatchChan
      forM_ (subtractV newQuery oldQuery) $ \patchQuery -> do
        resultPatch <- traverseWithKeyV handleV patchQuery
        flip runReaderT updateQueryResult' $ patchQueryResult resultPatch
      loop $ newQuery
    handleV
      :: forall f p. V f -> f p -> ReaderT r IO (f Identity)
    handleV gadtKey vessel = case gadtKey of
      V_Login ->
        fmap (fromMaybe $ MapV MM.empty) $ withConnection $ \conn ->
          liftIO $ runBeamSqlite conn $ do
            logins <- runSelectReturningList $ select $ do
              login <- all_ (_db_login db)
              guard_ $ in_
                (login ^. key)
                (fmap val_ $ MM.keys $ unMapV vessel)
              pure login
            pure $ MapV $ MM.fromList $ ffor logins $ \(Keyed k v) -> (k, Identity $ First $ Just v)
      V_Logins ->
        fmap (fromMaybe $ SingleV $ Identity $ First Nothing) $ withConnection $ \conn ->
          liftIO $ runBeamSqlite conn $ do
            logins <- runSelectReturningList $ select $ view key <$> all_ (_db_login db)
            pure $ SingleV $ Identity $ First $ Just $ S.fromList logins
      -- TODO
      V_Sync _ _ -> pure $ SingleV $ Identity $ First $ Nothing


readTChanConcat :: Semigroup a => TChan a -> IO a
readTChanConcat c = atomically (readTChan c) >>= concatWaiting
  where
    concatWaiting b = atomically (tryReadTChan c) >>= \case
      Just a -> concatWaiting $ a <> b
      Nothing -> return b

synchronize
  :: forall m r p
  .  ( HasJSContext m, MonadJSM m
     , MonadReader r m, HasLogger r
     )
  => (UserId, Login)
  -> SingleV SyncResponse p
  -> Maybe SyncBatchToken
  -> (SingleV SyncResponse Identity -> IO ())
  -> (Maybe SyncBatchToken -> IO ())
  -> m ()
synchronize (_userId, login) _query mSince0 updateQueryResult' signalSyncDone = do
  logger' <- asks $ view logger
  let
    qps = QPList_Cons Proxy (Just $ Filter'_Literal def)
        $ QPList_Cons Proxy mSince0
        $ QPList_Cons Proxy (Just True)
        $ QPList_Cons Proxy Nothing
        $ QPList_Cons Proxy Nothing
        $ QPList_Nil
  performRoutedRequest
    (ClientServerRoute_Event EventRoute_Sync)
    ("https://" <> _login_homeServer login)
    (_login_accessToken login)
    SyncRequest
    qps $
      flip runLoggingT logger' . \case
        Right (Right (XhrThisStatus SyncRespKey_200 (Right (Right result)))) -> do
          liftIO $ updateQueryResult' $ SingleV $ Identity $ First $ Just result
          $(logInfo) $ "Synced with server"
          liftIO $ signalSyncDone $ Just $ _syncResponse_nextBatch result
        _failure -> do
          $(logInfo) $ "Failed to synced with server "
          liftIO $ signalSyncDone Nothing

type X m t = 
       ( MonadFix m
       , MonadHold t m
       , PerformEvent t m
       , TriggerEvent t m
       )

type Y m t r =
       ( X m t
       , HasJSContext m
       , MonadJSM m
       )

runFrontendQueries
  :: forall m t js r a
  .  ( X m t
     , Y (Client m) t r
     , Y (Performable (Client m)) t r
     , Prerender js t m
     , PostBuild t m
     , GivesSQLiteConnection r, HasLogger r
     )
  => Dynamic t r
  -> ((FrontendV Identity -> IO ()) -> QueryT t (FrontendV (Const SelectedCount)) m a)
  -> m a
runFrontendQueries dctx m = do
  (queryResultPatch, updateQueryResult') <- newTriggerEvent
  (a, requestUniq :: Dynamic t FrontendQuery) <-
    flip cropQueryT queryResultPatch $ m updateQueryResult'
  let
    f = mapMaybeV (\(Const n) -> Proxy <$ guard (n >= 0))
    -- | Filter out all <= 0 values, and throw away count on rest; the
    -- frontend-backend doesn't need them.
    eRequestUniq :: Event t FrontendQuery'
    eRequestUniq = fforMaybe (updated requestUniq) $ f
    bRequestUniq :: Behavior t FrontendQuery'
    bRequestUniq = fromMaybe mempty . f <$> current requestUniq
  postBuild <- getPostBuild
  let updatedRequest = leftmost [eRequestUniq, tag bRequestUniq postBuild]
  prerender_ blank $ do
    queryPatchChan <- liftIO newTChanIO
    performEvent_ $ ffor updatedRequest $ \qp -> liftIO $
      atomically $ writeTChan queryPatchChan qp
    pb <- getPostBuild
    performEvent_ $ ffor (tag (current dctx) pb) $ \ctx ->
      liftIO $ void $ forkIO $ flip runReaderT ctx $
        handleQueryUpdates updateQueryResult' queryPatchChan
    (syncDone, signalSyncDone) <- newTriggerEvent
    syncDone' <- foldDyn M.union mempty syncDone
    let
      requestUniq' = unsafeBuildDynamic (sample bRequestUniq) updatedRequest
      filterSyncs
        :: VSum V f
        -> Maybe ((UserId, Login), SingleV SyncResponse f)
      filterSyncs (vtag :~> v) = case vtag of
        V_Sync userId login -> Just ((userId, login), v)
        _ -> Nothing
      queryAndThenSum = ffilter null
        $ updated
        $ M.intersectionWith (,)
        <$> (M.fromList . fmapMaybe filterSyncs . toListV <$> requestUniq')
        <*> syncDone'
    performEvent_ $ ffor queryAndThenSum $ \querys ->
      ifor_ querys $ \key'@(user, login) (query, mSince) ->
        synchronize key' query mSince
          (updateQueryResult' . singletonV (V_Sync user login))
          (signalSyncDone . M.singleton key')
  return a
