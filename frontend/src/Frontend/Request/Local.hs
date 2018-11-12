{-# LANGUAGE UndecidableInstances #-}
module Frontend.Request.Local where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Exception
import Control.Monad.Primitive
import Control.Monad.Ref
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Aeson
import Data.ByteString.Lazy (toStrict)
import Data.Coerce
import Data.Constraint
import Data.Text.Encoding
import Language.Javascript.JSaddle.Types
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Reflex.Host.Class

import Matrix.Client.Types

import Frontend.Request

data LocalFrontendRequestContext t = LocalFrontendRequestContext
  {
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
    ReaderT $ \c -> handleLocalFrontendRequest @js @t @m @(Performable m) k c r
  performFrontendRequest_ req = performEvent_ $ ffor req $ \r ->
    ReaderT $ \c -> handleLocalFrontendRequest @js @t @m @(Performable m) (const $ return ()) c r

handleLocalFrontendRequest
  :: forall js t m' m a. PrerenderPerformable js t m' m
  => (a -> IO ())
  -> LocalFrontendRequestContext t
  -> FrontendRequest a
  -> m ()
handleLocalFrontendRequest k _ = \case
  FrontendRequest_Login hs u pw -> do
    let handleResponse = \case
          Left e -> do
            print e
            k ()
          Right r -> do
            print $ decodeXhrResponse @LoginResponse r
            k ()
    let loginRequest = LoginRequest
          (UserIdentifier_User u)
          (Login_Password pw)
          Nothing
          Nothing
    let url = hs <> "/_matrix/client/r0/login"
    let body = decodeUtf8 $ toStrict $ Data.Aeson.encode loginRequest

    performRequestCallbackWithErrorPrerender @js @t @m' @m handleResponse $
      XhrRequest "POST" url $ def & xhrRequestConfig_sendData .~ body

performRequestCallbackWithError
  :: (MonadJSM m, HasJSContext m, IsXhrPayload a)
  => (Either XhrException XhrResponse -> IO ())
  -> XhrRequest a
  -> m ()
performRequestCallbackWithError k req =
  void $ newXMLHttpRequestWithError req $ liftIO . k

performRequestCallbackWithErrorPrerender
  :: forall js t m' m a
  .  (PrerenderPerformable js t m' m, IsXhrPayload a)
  => (Either XhrException XhrResponse -> IO ())
  -> XhrRequest a
  -> m ()
performRequestCallbackWithErrorPrerender =
  case prerenderClientDictPerformable @js @m' of
    Nothing -> (\_ _ -> return ())
    Just Dict -> performRequestCallbackWithError

type PrerenderPerformable js t m' m = (PerformEvent t m', Performable m' ~ m, Prerender js m')

type JSConstraints js m = (HasJS js m, MonadJSM m, HasJSContext m)

prerenderClientDictPerformable
  :: forall js m. Prerender js m
  => Maybe (Dict (JSConstraints js (Performable m)))
prerenderClientDictPerformable = ffor prerenderClientDict $
  \(Dict :: Dict (PrerenderClientConstraint js m)) -> Dict

runLocalFrontendRequestT :: LocalFrontendRequestT t m a -> m a
runLocalFrontendRequestT (LocalFrontendRequestT m) =
  runReaderT m LocalFrontendRequestContext
