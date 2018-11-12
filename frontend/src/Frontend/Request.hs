{-# LANGUAGE UndecidableInstances #-}
module Frontend.Request where

import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Exception
import Control.Monad.Primitive
import Control.Monad.Ref
import Control.Monad.Trans.Class
import Data.Coerce
import Data.Constraint
import Data.Functor.Identity
import Language.Javascript.JSaddle.Types
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Reflex.Host.Class

import Matrix.Client.Types

data FrontendRequest a where
  FrontendRequest_Login :: LoginResponse -> FrontendRequest ()

class MonadFrontendRequest t m | m -> t where
  performFrontendRequest :: Event t (FrontendRequest a) -> m (Event t a)
  performFrontendRequest_ :: Event t (FrontendRequest a) -> m ()

newtype FrontendRequestT t m a = FrontendRequestT
  { unFrontendRequestT :: RequesterT t FrontendRequest Identity m a
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
instance (MonadHold t m, MonadFix m, Adjustable t m) => Adjustable t (FrontendRequestT t m) where
  runWithReplace a0 a' = FrontendRequestT $ runWithReplace (coerce a0) (coerceEvent a')
  traverseDMapWithKeyWithAdjust f dm0 dm' = FrontendRequestT $ traverseDMapWithKeyWithAdjust (\k v -> unFrontendRequestT $ f k v) (coerce dm0) (coerceEvent dm')
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = FrontendRequestT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> unFrontendRequestT $ f k v) (coerce dm0) (coerceEvent dm')
  traverseIntMapWithKeyWithAdjust f dm0 dm' = FrontendRequestT $ traverseIntMapWithKeyWithAdjust (coerce . f) dm0 dm'

-- It looks like a GHC bug prevents this from being derived.
instance Prerender js m => Prerender js (FrontendRequestT t m) where
  prerenderClientDict = fmap (\Dict -> Dict) (prerenderClientDict :: Maybe (Dict (PrerenderClientConstraint js m)))

-- It looks like a GHC bug prevents this from being derived.
instance (Monad m, SetRoute t r m) => SetRoute t r (FrontendRequestT t m) where
  setRoute = lift . setRoute
  modifyRoute = lift . modifyRoute

-- It looks like a GHC bug prevents this from being derived.
instance (Monad m, RouteToUrl r m) => RouteToUrl r (FrontendRequestT t m) where
  askRouteToUrl = lift $ askRouteToUrl

-- TODO: Add the missing instance for RequesterT to reflex.
instance PrimMonad m => PrimMonad (FrontendRequestT t m) where
  type PrimState (FrontendRequestT t m) = PrimState m
  primitive = lift . primitive

instance (Monad m, MonadFrontendRequest t m) => MonadFrontendRequest t (RoutedT t r m) where
  performFrontendRequest = lift . performFrontendRequest
  performFrontendRequest_ = lift . performFrontendRequest_

instance (Reflex t, Monad m) => MonadFrontendRequest t (FrontendRequestT t m) where
  performFrontendRequest = FrontendRequestT . requestingIdentity
  performFrontendRequest_ = FrontendRequestT . requesting_

-- TODO: Handle requests.
runFrontendRequestT :: (Reflex t, Monad m) => FrontendRequestT t m a -> m a
runFrontendRequestT (FrontendRequestT m) = fst <$> runRequesterT m never
