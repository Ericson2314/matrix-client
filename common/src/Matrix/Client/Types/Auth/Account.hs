{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Matrix.Client.Types.Auth.Account where

import           Control.Lens hiding ((.=))
import           Control.Applicative (liftA2)
import           Control.Monad
import           Data.Aeson
import qualified Data.Aeson as Ae
import           Data.Aeson.Utils
import           Data.Constraint.Extras.TH
import           Data.DependentXhr
import           Data.Kind
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Some
import           Data.Text (Text)
import           Data.Traversable
import           Data.Int
import           Data.Word
import           Data.Void
import           GHC.Generics
import           Text.Megaparsec (Parsec, parseMaybe)
import           Text.URI

import           Matrix.Identifiers
import           Matrix.Client.Types.Common

--------------------------------------------------------------------------------

data AccountRoute httpType route needsAuth request respPerCode where
  AccountRoute_Register
    :: AccountRoute
       "POST"
       '[ 'Left "register" ]
       'False
       RegisterRequest
       RegisterRespKey
  AccountRoute_EmailRequestToken
    :: AccountRoute
       "POST"
       '[ 'Left "register", 'Left "email", 'Left "requestToken" ]
       'False
       EmailRequestTokenRequest
       EmailRequestTokenRespKey
  AccountRoute_PhoneEmailRequestToken
    :: AccountRoute
       "POST"
       '[ 'Left "register", 'Left "msisdn", 'Left "requestToken" ]
       'False
       PhoneRequestTokenRequest
       PhoneRequestTokenRespKey

--------------------------------------------------------------------------------

data RegisterRespKey :: Type -> Type where
  RegisterRespKey_200 :: RegisterRespKey RegisterResponse
  RegisterRespKey_401 :: RegisterRespKey Data.Aeson.Value
  RegisterRespKey_429 :: RegisterRespKey Data.Aeson.Value


data RegisterRequest = RegisterRequest
  { _registerRequest_auth :: AuthenticationData
  , _registerRequest_bindEmail :: Bool
  , _registerRequest_username :: UserName
  , _registerRequest_password :: Text
  , _registerRequest_deviceId :: Text
  , _registerRequest_initialDeviceDisplayName :: Text
  , _registerRequest_inhibitLogin :: Bool
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON RegisterRequest where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON RegisterRequest where
  toJSON = genericToJSON aesonOptions

-- | Should be empty record in most cases
data AuthenticationData = AuthenticationData
  deriving (Eq, Ord, Show, Generic)

instance FromJSON AuthenticationData where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON AuthenticationData where
  toJSON = genericToJSON aesonOptions

data RegisterResponse = RegisterResponse
  { _registerResponse_userId :: UserId
  , _registerResponse_accessToken :: AccessToken
  , _registerResponse_deviceId :: DeviceId
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON RegisterResponse where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON RegisterResponse where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data EmailRequestTokenRespKey :: Type -> Type where
  EmailRequestTokenRespKey_200 :: EmailRequestTokenRespKey EmailRequestTokenResponse
  EmailRequestTokenRespKey_403 :: EmailRequestTokenRespKey Data.Aeson.Value

data EmailRequestTokenRequest = EmailRequestTokenRequest
  { _emailRequestTokenRequest_clientSecret :: Text -- TODO there is a regex
  , _emailRequestTokenRequest_email :: Text -- TODO better type
  , _emailRequestTokenRequest_sendAttempt :: Word32 -- TODO width
  , _emailRequestTokenRequest_nextLink :: Maybe MatrixUri
  , _emailRequestTokenRequest_idServer :: ServerName
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON EmailRequestTokenRequest where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON EmailRequestTokenRequest where
  toJSON = genericToJSON aesonOptions

data EmailRequestTokenResponse = EmailRequestTokenResponse
  { _emailRequestTokenResponse_sid :: Text -- TODO regex and max len
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON EmailRequestTokenResponse where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON EmailRequestTokenResponse where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data PhoneRequestTokenRespKey :: Type -> Type where
  PhoneRequestTokenRespKey_200 :: PhoneRequestTokenRespKey PhoneRequestTokenResponse
  PhoneRequestTokenRespKey_403 :: PhoneRequestTokenRespKey Data.Aeson.Value

data PhoneRequestTokenRequest = PhoneRequestTokenRequest
  { _phoneRequestTokenRequest_clientSecret :: Text -- TODO there is a regex
  , _phoneRequestTokenRequest_country :: Text -- TODO better type
  -- phoneR ISO two letter code.
  , _phoneRequestTokenRequest_phoneNumber :: Text -- TODO better type
  , _phoneRequestTokenRequest_sendAttempt :: Word32 -- TODO width
  , _phoneRequestTokenRequest_nextLink :: Maybe MatrixUri
  , _phoneRequestTokenRequest_idServer :: ServerName
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON PhoneRequestTokenRequest where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON PhoneRequestTokenRequest where
  toJSON = genericToJSON aesonOptions

data PhoneRequestTokenResponse = PhoneRequestTokenResponse
  { _phoneRequestTokenResponse_sid :: Text -- TODO regex and max len
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON PhoneRequestTokenResponse where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON PhoneRequestTokenResponse where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

