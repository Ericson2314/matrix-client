{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Data.DependentXhr where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Utils
import           Data.ByteString.Lazy (toStrict, fromStrict)
--import           Data.Constraint (Dict (..))
import           Data.Constraint
import           Data.Constraint.Extras
import           Data.Constraint.Forall
import           Data.Default
import           Data.Kind
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Proxy
import           Data.Some
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import           Data.Void
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

data XhrSomeStatus (respPerCode :: Nat -> Type -> Type)
  =  forall status response
  .  KnownNat status
  => XhrThisStatus (respPerCode status response)
                   (Either ErrorXhrNoBody
                           (Either ErrorBadJsonParse response))

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


-- | Composition for constraints.
class p (f a) => ComposeC (p :: k2 -> Constraint) (f :: k1 -> k2) (a :: k1)
instance p (f a) => ComposeC p f a

class (ArgDict f, ConstraintsFor f c) => HasClass (c :: k -> Constraint) f
instance (ArgDict f, ConstraintsFor f c) => HasClass (c :: k -> Constraint) f

-- TODO GHC 8.6 use forall constraints. This is garbage sauce.
type RespTyConstr c respPerCode = Forall (ComposeC (HasClass c) respPerCode)

-- | Cribbed from singletons. (Though modulo names there's basically one way to
-- do this.)
data Decision a
  = Proved a
  | Disproved (a -> Void)
  deriving (Generic)

-- | Partially apply a relation (GADT) and then decide whether it's inhabitted
-- for any argument for the remaining parameter.
class DecidablableLookup (mapRel :: Nat -> k -> Type) where
  liftedLookup :: forall n. KnownNat n => Decision (Some (mapRel n))

performXhrCallbackWithErrorJSON
  :: forall js m request respPerCode
  .  ( JSConstraints js m
     , ToJSON request
     -- , GetStatusKey respPerCode
     , RespTyConstr FromJSON respPerCode
     , DecidablableLookup respPerCode
     )
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
    Right r -> Right $ case someNatVal $ fromIntegral $ _xhrResponse_status r of
      Nothing -> error "impossible, HTTP status are always positive, need a better `someNatVal`"
      Just (SomeNat (Proxy :: Proxy status)) -> case liftedLookup of
        Disproved _prf -> Left ErrorXhrInvalidStatus
        Proved (This (statusKey :: respPerCode status resp)) -> Right $ XhrThisStatus statusKey $
          case _xhrResponse_responseText r of
            Nothing -> Left ErrorXhrNoBody
            Just raw -> Right $ case inst of
              (Sub Dict :: Forall (ComposeC (HasClass FromJSON)
                                            respPerCode)
                        :- (ComposeC (HasClass FromJSON)
                                     respPerCode)
                           status) ->
                case has @FromJSON statusKey $ eitherDecode $ fromStrict $ encodeUtf8 raw of
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

data Method
  = GET
  | PUT
  | POST
  | DELETE
  deriving (Eq, Ord, Show, Generic, Enum)

methodToText :: Method -> Text
methodToText = \case
  GET -> "GET"
  PUT -> "PUT"
  POST -> "POST"
  DELETE -> "DELETE"

class ReifyMethod (m :: Method) where
  reifyMethod :: Method

instance ReifyMethod 'GET where
  reifyMethod = GET

instance ReifyMethod 'PUT where
  reifyMethod = PUT

instance ReifyMethod 'POST where
  reifyMethod = POST

instance ReifyMethod 'DELETE where
  reifyMethod = DELETE

type QueryParams = [(Symbol, Type)]

data QPList :: [(Symbol, Type)] -> Type where
  QPList_Nil
    :: QPList '[]
  QPList_Cons
    :: (KnownSymbol s, ToRoutePiece t)
    => Proxy s
    -> Maybe t -- all query params are optional for now
    -> QPList l
    -> QPList ('(s, t) : l)

type RespRelation = Nat -> Type -> Type

type Route
  =  Method
  -> RoutePath
  -> QueryParams
  -> Bool
  -> Type
  -> RespRelation
  -> Type

performRoutedRequest
  :: forall
       (routeRelation :: Route)
       js m
       (method :: Method)
       (route :: RoutePath)
       (queryParams :: QueryParams)
       (needsAuth :: Bool)
       request
       (respPerCode :: RespRelation)
  .  ( JSConstraints js m
     , KnownRoute route
     , KnownNeedsAuth needsAuth
     , ToJSON request
     -- , GetStatusKey respPerCode
     , RespTyConstr FromJSON respPerCode
     , DecidablableLookup respPerCode
     , ReifyMethod method
     )
  => routeRelation method route queryParams needsAuth request respPerCode
  -> Text -- ^ Home Server base URL
  -> (AuthFunctor
        needsAuth
        (request
         -> RouteFunctor route
                         (QPList queryParams
                          -> (XhrResponseParse respPerCode -> IO ())
                          -> m ())))
performRoutedRequest _c hs = _knownNeedsAuth_fmaplike @needsAuth
  (\mAuth r -> _knownRoute_fmaplike @route
    (\(routeList :: [Text]) (qps :: QPList queryParams) k -> do
        let
          method = methodToText $ reifyMethod @method
          f :: QPList qps -> [Text]
          f QPList_Nil =
            []
          f (QPList_Cons (Proxy :: Proxy sym) (mv :: Maybe v) qps') = case mv of
            Nothing -> rest
            Just v -> (reifyText @sym <> "=" <> toRoute v) : rest
            where rest = f qps'
          qparams :: Text
          qparams = case qps of
            QPList_Nil -> ""
            _ -> "?" <> T.intercalate "&" (f qps)
          url = (T.intercalate "/" $ hs : routeList) <> qparams
        performXhrCallbackWithErrorJSON @js @m @request @respPerCode
          method url mAuth r k)
    (reifyRoute @route))
  (makeToken @needsAuth)
