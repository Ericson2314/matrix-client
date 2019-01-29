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
import           Data.Semigroup
import qualified Data.Set as S
import           Data.Vessel
import           Database.Beam
import           Database.Beam.Sqlite
import           Obelisk.Database.Beam.Entity
import           Reflex.Dom.Core hiding (Key, select)
-- TODO move to reflex. This method doesn't use web stuff.
import           Reflex.Dom.WebSocket.Query (cropQueryT)
import           Reflex.Dom.Prerender.Performable

import           Matrix.Identifiers
import           Matrix.Client.Types hiding (Login)

import           Data.DependentXhr
import           Frontend.Query
import           Frontend.Schema
import           Frontend.Backend.Common

handleQueryUpdates
  :: forall r
  .  GivesSQLiteConnection r
  => (FrontendV Identity -> IO ())
  -> TChan (FrontendV (Const SelectedCount))
  -> ReaderT r IO ()
handleQueryUpdates updateQueryResult' queryPatchChan = do
  forever $ do
    patch <- liftIO $ readTChanConcat queryPatchChan
    -- TODO: Crop out negative selected counts in patch.
    patch' <- traverseWithKeyV handleV patch
    flip runReaderT updateQueryResult' $ patchQueryResult patch'
 where
    handleV
      :: forall f p. V f -> f p -> ReaderT r IO (f Identity)
    handleV gadtKey vessel = case gadtKey of
      V_Login ->
        fmap (fromMaybe $ MapV MM.empty) $ withConnection $ \conn ->
          liftIO $ runBeamSqlite conn $ do
            logins <- runSelectReturningList $ select $ do
              login <- all_ (_db_login db)
              guard_ $ in_
                (getId $ _entity_key login)
                (fmap (val_ . getId) $ MM.keys $ unMapV vessel)
              pure login
            pure $ MapV $ MM.fromList $ ffor logins $ \(Entity k v) -> (k, Identity $ First $ Just v)
      V_Logins ->
        fmap (fromMaybe $ SingleV $ Identity $ First Nothing) $ withConnection $ \conn ->
          liftIO $ runBeamSqlite conn $ do
            logins <- runSelectReturningList $ select $ _entity_key <$> all_ (_db_login db)
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
  :: forall js m r p
  .  (JSConstraints js m, MonadReader r m, HasLogger r)
  => (UserId, Login)
  -> SingleV SyncResponse p
  -> Maybe SyncBatchToken
  -> (SingleV SyncResponse Identity -> IO ())
  -> (Maybe SyncBatchToken -> IO ())
  -> m ()
synchronize (userId, login) query mSince0 updateQueryResult' signalSyncDone = do
  logger' <- asks $ view logger
  let
    qps = QPList_Cons Proxy Nothing
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

runFrontendQueries
  :: forall m t js r a
  .  ( MonadFix m
     , MonadHold t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadIO m
     , Prerender js m
     , PostBuild t m
     , MonadReader r m, GivesSQLiteConnection r, HasLogger r
     )
  => ((FrontendV Identity -> IO ())
      -> QueryT t (FrontendV (Const SelectedCount)) m a)
  -> m a
runFrontendQueries m = do
  ctx <- ask
  (queryResultPatch, updateQueryResult') <- newTriggerEvent
  (a, requestUniq) <- flip cropQueryT queryResultPatch $ (m updateQueryResult')
  postBuild <- getPostBuild
  let updatedRequest = AdditivePatch <$>
        leftmost [updated requestUniq, tag (current requestUniq) postBuild]
  prerender blank $ do
    queryPatchChan <- liftIO newTChanIO
    performEvent_ $ ffor updatedRequest $ \qp -> liftIO $
      atomically $ writeTChan queryPatchChan $ unAdditivePatch qp
    liftIO $ void $ forkIO $ flip runReaderT ctx $
      handleQueryUpdates updateQueryResult' queryPatchChan
  flip runReaderT ctx $ prerender blank $ do
    (syncDone, signalSyncDone) <- newTriggerEvent
    syncDone' <- foldDyn M.union mempty syncDone
    let filterSyncs
          :: VSum V f
          -> Maybe ((UserId, Login), SingleV SyncResponse f)
        filterSyncs (vtag :~> v) = case vtag of
          V_Sync userId login -> Just ((userId, login), v)
          _ -> Nothing
        queryAndThenSum = ffilter null
          $ updated
          $ M.intersectionWith (,)
          <$> (M.fromList . fmapMaybe filterSyncs . toListV <$> requestUniq)
          <*> syncDone'
    performEvent_ $ ffor queryAndThenSum $ \querys ->
      ifor_ querys $ \key@(user, login) (query, mSince) ->
        synchronize @js key query mSince
          (updateQueryResult' . singletonV (V_Sync user login))
          (signalSyncDone . M.singleton key)
  return a