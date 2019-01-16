{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}
module Matrix.Client.Types.Auth.Account
  ( module Matrix.Client.Types.Auth.Account
  , module Matrix.Client.Types.Auth.Account.ThirdParty
  ) where

import           Control.Lens hiding ((.=))
import           Control.Applicative (liftA2)
import           Control.Monad
import           Data.Aeson
import           Data.Constraint.Extras.TH
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

data AccountRoute :: Route where
  AccountRoute_Register
    :: AccountRoute
       'POST
       '[ 'Left "register" ]
       '[ ]
       'False
       RegisterRequest
       RegisterRespKey
  AccountRoute_EmailRequestToken
    :: AccountRoute
       'POST
       '[ 'Left "register", 'Left "email", 'Left "requestToken" ]
       '[ ]
       'False
       EmailRequestTokenRequest
       EmailRequestTokenRespKey
  AccountRoute_PhoneEmailRequestToken
    :: AccountRoute
       'POST
       '[ 'Left "register", 'Left "msisdn", 'Left "requestToken" ]
       '[ ]
       'False
       PhoneRequestTokenRequest
       PhoneRequestTokenRespKey
  AccountRoute_Deactivate
    :: AccountRoute
       'POST
       '[ 'Left "account", 'Left "deactivate" ]
       '[ ]
       'True
       DeactivateRequest
       DeactivateRespKey
  AccountRoute_Available
    :: AccountRoute
       'GET
       '[ 'Left "register", 'Left "available" ]
       '[ ]
       'False
       AvailableRequest
       AvailableRespKey
  AccountRoute_ThirdParty
    :: Account3PIdRoute httpType route queryParams needsAuth request respPerCode
    -> AccountRoute
       httpType
       ('Left "account" ': 'Left "3pid" ': route)
       queryParams
       needsAuth
       request
       respPerCode
  AccountRoute_WhoAmI
    :: AccountRoute
       'GET
       '[ 'Left "register", 'Left "available" ]
       '[ ]
       'True
       WhoAmIRequest
       WhoAmIRespKey

--------------------------------------------------------------------------------

data RegisterRespKey :: RespRelation where
  RegisterRespKey_200 :: RegisterRespKey 200 RegisterResponse
  RegisterRespKey_401 :: RegisterRespKey 401 Data.Aeson.Value
  RegisterRespKey_429 :: RegisterRespKey 429 Data.Aeson.Value


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

data DeactivateRespKey :: RespRelation where
  DeactivateRespKey_200 :: DeactivateRespKey 200 DeactivateResponse
  DeactivateRespKey_429 :: DeactivateRespKey 429 Data.Aeson.Value

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

data AvailableRespKey :: RespRelation where
  AvailableRespKey_200 :: AvailableRespKey 200 AvailableResponseAvailable
  AvailableRespKey_400 :: AvailableRespKey 400 AvailableResponseUnavailable
  AvailableRespKey_429 :: AvailableRespKey 429 Data.Aeson.Value

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

data WhoAmIRespKey :: RespRelation where
  WhoAmIRespKey_200 :: WhoAmIRespKey 200 WhoAmIResponse
  WhoAmIRespKey_401 :: WhoAmIRespKey 401 Data.Aeson.Value
  WhoAmIRespKey_403 :: WhoAmIRespKey 403 Data.Aeson.Value
  WhoAmIRespKey_429 :: WhoAmIRespKey 429 Data.Aeson.Value

data WhoAmIRequest = WhoAmIRequest
  deriving (Eq, Ord, Show, Generic)

instance FromJSON WhoAmIRequest where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON WhoAmIRequest where
  toJSON = genericToJSON aesonOptions

data WhoAmIResponse = WhoAmIResponse
  { _whoAmIResponse_userId :: UserId
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON WhoAmIResponse where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON WhoAmIResponse where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

join <$> traverse (deriveArgDict)
  [ ''RegisterRespKey
  , ''DeactivateRespKey
  , ''AvailableRespKey
  , ''WhoAmIRespKey
  ]

join <$> traverse (\ty -> liftA2 (<>) (makeLenses ty) (makeFields ty))
  [ ''RegisterRequest, ''RegisterResponse
  , ''DeactivateRequest, ''DeactivateResponse
  , ''AvailableRequest, ''AvailableResponseAvailable, ''AvailableResponseUnavailable
  , ''WhoAmIRequest, ''WhoAmIResponse
  ]
