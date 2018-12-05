{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.DependentXhr where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Utils
import           Data.ByteString.Lazy (toStrict, fromStrict)
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

data ErrorXhrInvalidStatus = ErrorXhrInvalidStatus
  deriving (Eq, Ord, Show)

class GetStatusKey (respPerCode :: Type -> Type) where
  lookupStatus :: Word16 -> Either ErrorXhrInvalidStatus (Some respPerCode)

data XhrSomeStatus (typePerCode :: Type -> Type)
  = forall r. XhrThisStatus (typePerCode r)
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

reifyText :: forall (t :: Symbol). KnownSymbol t => Text
reifyText = T.pack $ symbolVal $ Proxy @t

performRoutedRequest
  :: forall routeRelation js m ty route request respPerCode
  .  ( JSConstraints js m, KnownSymbol route, KnownSymbol ty
     , ToJSON request, GetStatusKey respPerCode, Has FromJSON respPerCode)
  => routeRelation ty route request respPerCode
  -> request
  -> Text -- ^ Home Server base URL
  -> (XhrResponseParse respPerCode -> IO ())
  -> m ()
performRoutedRequest _c r hs k = performXhrCallbackWithErrorJSON
  @js @m @request @respPerCode (reifyText @ty) (hs <> reifyText @route) r k
