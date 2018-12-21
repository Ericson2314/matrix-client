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
import           Data.Proxy
import           Data.Some
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import           Data.Word
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
  lookupStatus :: Word16 -> Either ErrorXhrInvalidStatus (Some respPerCode)

data XhrSomeStatus (respPerCode :: Type -> Type)
  = forall r. XhrThisStatus (respPerCode r)
                            (Either ErrorXhrNoBody
                                    (Either ErrorBadJsonParse r))

type XhrResponseParse respPerCode =
  Either XhrException
         (Either ErrorXhrInvalidStatus
                 (XhrSomeStatus respPerCode))

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
  -> request
  -> (XhrResponseParse respPerCode -> IO ())
  -> m ()
performXhrCallbackWithErrorJSON method url request k = do
  let
    body = decodeUtf8 $ toStrict $ Data.Aeson.encode request
    req = XhrRequest method url $ def & xhrRequestConfig_sendData .~ body
  flip performRequestCallbackWithError req $ k . \case
    Left e -> Left e
    Right r -> Right $ case lookupStatus $ fromIntegral $ _xhrResponse_status r of
      Left ErrorXhrInvalidStatus -> Left ErrorXhrInvalidStatus
      Right (This statusKey) -> Right $ XhrThisStatus statusKey $
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
  _fmaplike :: (x -> y) -> RouteFunctor route x -> RouteFunctor route y
  reifyRoute :: RouteFunctor route [Text]

instance KnownRoute '[] where
  type RouteFunctor '[] x = x
  _fmaplike = id
  reifyRoute = []

instance (KnownRoute r, KnownSymbol s) => KnownRoute ('Left s : r) where
  type RouteFunctor ('Left s : r) x = RouteFunctor r x
  _fmaplike = _fmaplike @r
  reifyRoute = _fmaplike @r (reifyText @s :) (reifyRoute @r)

instance (KnownRoute r, ToRoutePiece t) => KnownRoute ('Right t : r) where
  type RouteFunctor ('Right t : r) x = t -> RouteFunctor r x
  _fmaplike = fmap . _fmaplike @r
  reifyRoute t = _fmaplike @r (toRoute t :) (reifyRoute @r)

performRoutedRequest
  :: forall routeRelation js m ty (route :: RoutePath) request respPerCode
  .  ( JSConstraints js m, KnownSymbol ty, KnownRoute route
     , ToJSON request, GetStatusKey respPerCode, Has FromJSON respPerCode)
  => routeRelation ty route request respPerCode
  -> Text -- ^ Home Server base URL
  -> request
  -> RouteFunctor route ((XhrResponseParse respPerCode -> IO ()) -> m ())
performRoutedRequest _c hs r = _fmaplike @route
  (\(routeList :: [Text]) k -> performXhrCallbackWithErrorJSON
    @js @m @request @respPerCode (reifyText @ty) (T.intercalate "/" $ hs : routeList) r k)
  (reifyRoute @route)

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
