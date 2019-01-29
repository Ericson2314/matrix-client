{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeInType #-}
module Frontend.Backend.RequestHandler where

import           Control.Lens hiding (has)
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Logger
import           Data.DependentXhr
import qualified Data.Map.Monoidal as MM
import           Data.Semigroup
import qualified Data.Set as S
import           Data.Vessel
import           Database.Beam
import           Database.Beam.Sqlite
import           GHC.TypeLits
import           Obelisk.Database.Beam.Entity
import           Reflex.Dom.Prerender.Performable

import           Matrix.Client.Types as M hiding (Event)
import           Matrix.Identifiers as M

import           Frontend.Query
import           Frontend.Request
import           Frontend.Schema
import           Frontend.Backend.Common

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
  :: forall js m a r
  .  ( JSConstraints js m, MonadReader r m
     , GivesSQLiteConnection r
     , HasQueryCallback r
     , HasLogger r
     )
  => FrontendRequest a
  -> (a -> LoggingT (ReaderT r IO) ())
  -> m ()
handleLocalFrontendRequest req0 k = do
  ctx <- ask
  logger' <- asks $ view logger
  let
    -- | Arbitrary combinator soup. 'convertErrors' does the bulk of the work; the
    -- rest here is just whatever happens to be the common pattern for
    -- `handleLocalFrontendRequest`.
    cvtE
      :: forall respPerCode e b.  a ~ Either (FrontendError e) b
      => (forall s resp
          . KnownNat s
          => respPerCode s resp
          -> resp
          -> LoggingT (ReaderT r IO) (Either (FrontendError e) b))
      -> XhrResponseParse respPerCode
      -> IO ()
    cvtE k' = flip runReaderT ctx . flip runLoggingT logger' . (k <=< convertErrors k')
  case req0 of
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
                , _login_accessToken = r ^. loginResponse_accessToken
                , _login_deviceId = Just $ r ^. loginResponse_deviceId
                , _login_isActive = True
                }
              newEntity = Entity uid' newValue
          lids <- withConnection $ \conn -> liftIO $ runBeamSqlite conn $ do
            -- TODO: Add upsert support for beam-sqlite.
            old <- runSelectReturningOne $ lookup_ (_db_login db) $ EntityKey uid'
            runUpdate $ update (_db_login db)
              (\login -> (login ^. entity_value . login_isActive) <-. val_ False)
              (\_ -> val_ True)
            case old of
              Nothing -> runInsert $ insert (_db_login db) $ insertValues [newEntity]
              Just _ -> runUpdate $ save (_db_login db) newEntity
            runSelectReturningList $ select $ _entity_key <$> all_ (_db_login db)
          $(logInfo) $ "Sucessfully logged in user: " <> printUserId uid
          lift $ patchQueryResult $ mconcat
            [ singletonV V_Login $ MapV $
                MM.singleton uid' $ Identity $ First $ Just newValue
            , singletonV V_Logins $ SingleV $ Identity $ First $
                S.fromList <$> lids
            ]
          pure $ Right $ r ^. loginResponse_accessToken
    FrontendRequest_ListPublicRooms hs limit since -> do
      let
        hsText = "https://" <> printServerName hs
        qps = QPList_Cons Proxy (Just limit)
            $ QPList_Cons Proxy since
            $ QPList_Cons Proxy Nothing
            $ QPList_Nil
      performRoutedRequest (ClientServerRoute_Room RoomRoute_PublicRooms) hsText PublicRoomsRequest qps $ cvtE $ \sentinal r -> case sentinal of
        PublicRoomsRespKey_200 -> do
          $(logInfo) $ "Got some publicly listed rooms"
          pure $ Right r
    FrontendRequest_JoinRoom hs token room -> do
      let hsText = "https://" <> printServerName hs
      performRoutedRequest (ClientServerRoute_Room RoomRoute_Join) hsText token (JoinRequest Nothing) room QPList_Nil $ cvtE $ \sentinal r -> case sentinal of
        JoinRespKey_403 -> pure $ Left $ FrontendError_ResponseError r
        JoinRespKey_429 -> pure $ Left $ FrontendError_ResponseError r
        JoinRespKey_200 -> do
          $(logInfo) $ "Sucessfully join room: " <> printRoomId room
          pure $ Right ()
  where
