{-# LANGUAGE UndecidableInstances #-}
module Frontend.Request.Local where

import Control.Concurrent
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Exception
import Control.Monad.Primitive
import Control.Monad.Ref
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Coerce
import Data.Constraint
import Language.Javascript.JSaddle.Types
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Reflex.Host.Class

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

-- TODO: Add the missing instance for RequesterT to reflex.
instance PrimMonad m => PrimMonad (LocalFrontendRequestT t m) where
  type PrimState (LocalFrontendRequestT t m) = PrimState m
  primitive = lift . primitive

instance (Reflex t, PerformEvent t m, MonadIO (Performable m), TriggerEvent t m) => MonadFrontendRequest t (LocalFrontendRequestT t m) where
  performFrontendRequest req = performEventAsync $ ffor req $ \r k ->
    liftIO $ void $ forkIO $ k =<< handleLocalFrontendRequest r
  performFrontendRequest_ req = performEvent_ $ ffor req $
    liftIO . void . forkIO . void . handleLocalFrontendRequest

handleLocalFrontendRequest :: Monad m => FrontendRequest a -> m a
handleLocalFrontendRequest = \case
  FrontendRequest_Login _ -> return ()

-- TODO: Handle requests.
runLocalFrontendRequestT :: LocalFrontendRequestT t m a -> m a
runLocalFrontendRequestT (LocalFrontendRequestT m) =
  runReaderT m LocalFrontendRequestContext
