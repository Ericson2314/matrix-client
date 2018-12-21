{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.DependentXhr where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Utils
import           Data.ByteString.Lazy (toStrict, fromStrict)
--import           Data.Constraint (Dict (..))
import           Data.Constraint.Extras
import           Data.Kind
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Maybe (fromMaybe)
import           Data.Proxy
import           Data.Some
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import           Data.Word
import           GHC.Generics
import           GHC.TypeLits
import           Language.Javascript.JSaddle.Types
import           Reflex.Dom.Core
import           Reflex.Dom.Prerender.Performable

data ErrorXhrNoBody = ErrorXhrNoBody
  deriving (Eq, Ord, Show)

-- Indicates an unexpected response code. In idiomatic usage, this means one
-- cannot decode the body automatically because it is unclear what type to
-- decode to.
data ErrorXhrInvalidStatus = ErrorXhrInvalidStatus
  deriving (Eq, Ord, Show)

-- | Maps a HTTP response code the type to which the the response should
-- decode to.
class GetStatusKey (respPerCode :: Type -> Type) where
  statusMap :: Map Word16 (Some respPerCode)

data XhrSomeStatus (respPerCode :: Type -> Type)
  = forall r. XhrThisStatus (respPerCode r)
                            (Either ErrorXhrNoBody
                                    (Either ErrorBadJsonParse r))

type XhrResponseParse respPerCode =
  Either XhrException
         (Either ErrorXhrInvalidStatus
                 (XhrSomeStatus respPerCode))

newtype AccessToken = AccessToken { getAccessToken :: Text}
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON)
  deriving (Generic)

performRequestCallbackWithError
  :: forall m a. (MonadJSM m, HasJSContext m, IsXhrPayload a)
  => (Either XhrException XhrResponse -> IO ())
  -> XhrRequest a
  -> m ()
performRequestCallbackWithError k req =
  void $ newXMLHttpRequestWithError req $ liftIO . k

performXhrCallbackWithErrorJSON
  :: forall js m request respPerCode
  .  (JSConstraints js m, ToJSON request, GetStatusKey respPerCode, Has FromJSON respPerCode)
  => Text
  -> Text
  -> Maybe AccessToken
  -> request
  -> (XhrResponseParse respPerCode -> IO ())
  -> m ()
performXhrCallbackWithErrorJSON method url auth request k = do
  let
    body = decodeUtf8 $ toStrict $ Data.Aeson.encode request
    req = XhrRequest method url $ def
      & xhrRequestConfig_sendData .~ body
      & xhrRequestConfig_headers .~ (fromMaybe mempty $ Map.singleton "Authorization" . getAccessToken <$> auth)
  flip performRequestCallbackWithError req $ k . \case
    Left e -> Left e
    Right r -> Right $ case Map.lookup (fromIntegral $ _xhrResponse_status r) statusMap of
      Nothing -> Left ErrorXhrInvalidStatus
      Just (This statusKey) -> Right $ XhrThisStatus statusKey $
        case _xhrResponse_responseText r of
          Nothing -> Left ErrorXhrNoBody
          Just raw -> Right $ case has @FromJSON statusKey $ eitherDecode $ fromStrict $ encodeUtf8 raw of
            Left jsonError -> Left $ ErrorBadJsonParse jsonError raw
            Right val -> Right val

type RoutePath = [Either Symbol Type]

reifyText :: forall (t :: Symbol). KnownSymbol t => Text
reifyText = T.pack $ symbolVal $ Proxy @t

class ToRoutePiece t where
  toRoute :: t -> Text

class KnownRoute (route :: RoutePath) where
  type RouteFunctor route x :: Type
  _knownRoute_fmaplike :: (x -> y) -> RouteFunctor route x -> RouteFunctor route y
  reifyRoute :: RouteFunctor route [Text]

instance KnownRoute '[] where
  type RouteFunctor '[] x = x
  _knownRoute_fmaplike = id
  reifyRoute = []

instance (KnownRoute r, KnownSymbol s) => KnownRoute ('Left s : r) where
  type RouteFunctor ('Left s : r) x = RouteFunctor r x
  _knownRoute_fmaplike = _knownRoute_fmaplike @r
  reifyRoute = _knownRoute_fmaplike @r (reifyText @s :) (reifyRoute @r)

instance (KnownRoute r, ToRoutePiece t) => KnownRoute ('Right t : r) where
  type RouteFunctor ('Right t : r) x = t -> RouteFunctor r x
  _knownRoute_fmaplike = fmap . _knownRoute_fmaplike @r
  reifyRoute t = _knownRoute_fmaplike @r (toRoute t :) (reifyRoute @r)

class KnownNeedsAuth (needsAuth :: Bool) where
  type AuthFunctor needsAuth x :: Type
  _knownNeedsAuth_fmaplike :: (x -> y) -> AuthFunctor needsAuth x -> AuthFunctor needsAuth y
  makeToken :: AuthFunctor needsAuth (Maybe AccessToken)

instance KnownNeedsAuth 'False where
  type AuthFunctor 'False x = x
  _knownNeedsAuth_fmaplike = id
  makeToken = Nothing

instance KnownNeedsAuth 'True where
  type AuthFunctor 'True x = AccessToken -> x
  _knownNeedsAuth_fmaplike = fmap
  makeToken = Just

performRoutedRequest
  :: forall routeRelation js m ty (route :: RoutePath) (needsAuth :: Bool) request respPerCode
  .  ( JSConstraints js m, KnownSymbol ty, KnownRoute route, KnownNeedsAuth needsAuth
     , ToJSON request, GetStatusKey respPerCode, Has FromJSON respPerCode)
  => routeRelation ty route needsAuth request respPerCode
  -> Text -- ^ Home Server base URL
  -> (AuthFunctor needsAuth
      (request
       -> RouteFunctor route ((XhrResponseParse respPerCode -> IO ())
                              -> m ())))
performRoutedRequest _c hs = _knownNeedsAuth_fmaplike @needsAuth
  (\mAuth r -> _knownRoute_fmaplike @route
    (\(routeList :: [Text]) k -> performXhrCallbackWithErrorJSON
      @js @m @request @respPerCode (reifyText @ty) (T.intercalate "/" $ hs : routeList) mAuth r k)
    (reifyRoute @route))
  (makeToken @needsAuth)

-- type family Lefts (s :: [Either a b]) :: [a] where
--   Lefts '[] = '[]
--   Lefts ('Right _ : r) = Lefts r
--   Lefts ('Left s : r) = s : Lefts r
--
-- type family Rights (s :: [Either a b]) :: [b] where
--   Rights '[] = '[]
--   Rights ('Left _ : r) = Rights r
--   Rights ('Right s : r) = s : Rights r
--
-- type family ListHas (c :: x -> Constraint) (f :: [x]) :: Constraint where
--   ListHas _ '[] = ()
--   ListHas c (s : r) = (c s, ListHas c r)
--
-- _test
--   :: forall (route :: RoutePath)
--   .  ( ListHas KnownSymbol (Lefts route)
--      , ListHas ToJSON (Rights route)
--      )
--   => Dict (KnownRoute route)
-- _test = Dict
