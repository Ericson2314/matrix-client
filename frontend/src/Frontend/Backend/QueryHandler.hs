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
import qualified Data.Map.Monoidal as MM
import           Data.Maybe
import           Data.Semigroup
import qualified Data.Set as S
import           Data.Vessel
import           Database.Beam
import           Database.Beam.Sqlite
import           Obelisk.Database.Beam.Entity
import           Reflex.Dom.Core hiding (select)
-- TODO move to reflex. This method doesn't use web stuff.
import           Reflex.Dom.WebSocket.Query (cropQueryT)

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
    return ()
 where
    handleV
      :: forall f p. V f -> f p -> ReaderT r IO (f Identity)
    handleV gadtKey vessel = case gadtKey of
      V_Login ->
        fmap (fromMaybe $ MapV MM.empty) $ withConnection $ \conn ->
          liftIO $ runBeamSqlite conn $ do
            logins <- runSelectReturningList $ select $ do
              login <- all_ (dbLogin db)
              guard_ $ in_
                (getId $ _entity_key login)
                (fmap (val_ . getId) $ MM.keys $ unMapV vessel)
              pure login
            pure $ MapV $ MM.fromList $ ffor logins $ \(Entity k v) -> (k, Identity $ First $ Just v)
      V_Logins ->
        fmap (fromMaybe $ SingleV $ Identity $ First Nothing) $ withConnection $ \conn ->
          liftIO $ runBeamSqlite conn $ do
            logins <- runSelectReturningList $ select $ _entity_key <$> all_ (dbLogin db)
            pure $ SingleV $ Identity $ First $ Just $ S.fromList logins

readTChanConcat :: Semigroup a => TChan a -> IO a
readTChanConcat c = atomically (readTChan c) >>= concatWaiting
  where
    concatWaiting b = atomically (tryReadTChan c) >>= \case
      Just a -> concatWaiting $ a <> b
      Nothing -> return b

runFrontendQueries
  :: ( MonadFix m
     , MonadHold t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadIO m
     , Prerender js m
     , PostBuild t m
     , MonadReader r m, GivesSQLiteConnection r
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
  return a
