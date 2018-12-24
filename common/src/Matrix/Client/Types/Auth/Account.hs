{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Matrix.Client.Types.Auth.Account
  ( module Matrix.Client.Types.Auth.Account
  , module Matrix.Client.Types.Auth.Account.ThirdParty
  ) where

import           Control.Lens hiding ((.=))
import           Control.Applicative (liftA2)
import           Control.Monad
import           Data.Aeson
import           Data.Constraint.Extras.TH
import           Data.Kind
import           Data.Text (Text)
import           GHC.Generics

import           Data.DependentXhr

import           Matrix.Identifiers
import           Matrix.Client.Types.Common
import           Matrix.Client.Types.Auth.Account.ThirdParty

--------------------------------------------------------------------------------

data AuthenticationData = AuthenticationData
  { _authenticationData_type :: Text
  , _authenticationData_session :: Maybe Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON AuthenticationData where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON AuthenticationData where
  toJSON = genericToJSON aesonOptions

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
  AccountRoute_Deactivate
    :: AccountRoute
       "POST"
       '[ 'Left "account", 'Left "deactivate" ]
       'True
       DeactivateRequest
       DeactivateRespKey
  AccountRoute_Available
    :: AccountRoute
       "GET"
       '[ 'Left "register", 'Left "available" ]
       'False
       AvailableRequest
       AvailableRespKey
  AccountRoute_ThirdParty
    :: Account3PIdRoute httpType route needsAuth request respPerCode
    -> AccountRoute
       httpType
       ('Left "account" ': 'Left "3pid" ': route)
       needsAuth
       request
       respPerCode

--------------------------------------------------------------------------------

data RegisterRespKey :: Type -> Type where
  RegisterRespKey_200 :: RegisterRespKey RegisterResponse
  RegisterRespKey_401 :: RegisterRespKey Data.Aeson.Value
  RegisterRespKey_429 :: RegisterRespKey Data.Aeson.Value


data RegisterRequest = RegisterRequest
  { _registerRequest_auth :: Maybe AuthenticationData
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

data DeactivateRespKey :: Type -> Type where
  DeactivateRespKey_200 :: DeactivateRespKey DeactivateResponse
  DeactivateRespKey_429 :: DeactivateRespKey Data.Aeson.Value

data DeactivateRequest = DeactivateRequest
  { _deactivateRequest_auth :: AuthenticationData
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON DeactivateRequest where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON DeactivateRequest where
  toJSON = genericToJSON aesonOptions

data DeactivateResponse = DeactivateResponse
  deriving (Eq, Ord, Show, Generic)

instance FromJSON DeactivateResponse where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON DeactivateResponse where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data AvailableRespKey :: Type -> Type where
  AvailableRespKey_200 :: AvailableRespKey AvailableResponseAvailable
  AvailableRespKey_400 :: AvailableRespKey AvailableResponseUnavailable
  AvailableRespKey_429 :: AvailableRespKey Data.Aeson.Value

data AvailableRequest = AvailableRequest
  { _availableRequest_username :: UserName
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON AvailableRequest where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON AvailableRequest where
  toJSON = genericToJSON aesonOptions

data AvailableResponseAvailable = AvailableResponseAvailable
  { _availableResponseAvailable_available :: Bool
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON AvailableResponseAvailable where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON AvailableResponseAvailable where
  toJSON = genericToJSON aesonOptions

-- TODO this is just one example of the common pattern of an error code with
-- message. Should factor out and newtype.
data AvailableResponseUnavailable = AvailableResponseUnvailable
  { _availableResponseUnavailable_errorCode :: Text -- TODO enum
  , _availableResponseUnavailable_error :: Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON AvailableResponseUnavailable where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON AvailableResponseUnavailable where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

join <$> traverse deriveArgDict
  [ ''RegisterRespKey
  , ''DeactivateRespKey
  , ''AvailableRespKey
  ]

join <$> traverse (\ty -> liftA2 (<>) (makeLenses ty) (makeFields ty))
  [ ''RegisterRequest, ''RegisterResponse
  , ''DeactivateRequest, ''DeactivateResponse
  , ''AvailableRequest, ''AvailableResponseAvailable, ''AvailableResponseUnavailable
  ]
