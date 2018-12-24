{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Matrix.Client.Types.Auth.Login where

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

data LoginRoute httpType route needsAuth request respPerCode where
  LoginRoute_Login'
    :: LoginRoute
       "GET"
       '[ 'Left "login" ]
       'False
       Login'Request
       Login'RespKey
  LoginRoute_Login
    :: LoginRoute
       "POST"
       '[ 'Left "login" ]
       'False
       LoginRequest
       LoginRespKey
  LoginRoute_Logout
    :: LoginRoute
       "POST"
       '[ 'Left "logout" ]
       'True
       LogoutRequest
       LogoutRespKey
  LoginRoute_LogoutAll
    :: LoginRoute
       "POST"
       '[ 'Left "logout", 'Left "all" ]
       'True
       LogoutAllRequest
       LogoutAllRespKey

--------------------------------------------------------------------------------

data Login'RespKey :: Type -> Type where
  Login'RespKey_200 :: Login'RespKey Login'Response
  Login'RespKey_400 :: Login'RespKey Data.Aeson.Value
  Login'RespKey_403 :: Login'RespKey Data.Aeson.Value
  Login'RespKey_429 :: Login'RespKey Data.Aeson.Value

data Login'Request = Login'Request
  deriving (Eq, Ord, Show, Generic)

instance FromJSON Login'Request where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON Login'Request where
  toJSON = genericToJSON aesonOptions

data Login'Response = Login'Response
  { _login'Request_flows :: [LoginFlow]
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON Login'Response where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON Login'Response where
  toJSON = genericToJSON aesonOptions

data LoginFlow = LoginFlow
  { _loginFlow_type :: Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON LoginFlow where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON LoginFlow where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data LoginRespKey :: Type -> Type where
  LoginRespKey_200 :: LoginRespKey LoginResponse
  LoginRespKey_400 :: LoginRespKey Data.Aeson.Value
  LoginRespKey_403 :: LoginRespKey Data.Aeson.Value
  LoginRespKey_429 :: LoginRespKey Data.Aeson.Value

instance GetStatusKey LoginRespKey where
  statusMap = Map.fromList
    [ (200, This LoginRespKey_200)
    , (400, This LoginRespKey_400)
    , (403, This LoginRespKey_403)
    , (429, This LoginRespKey_429)
    ]

data LoginRequest = LoginRequest
  { _loginRequest_identifier :: UserIdentifier
  , _loginRequest_login :: Login
  , _loginRequest_deviceId :: Maybe DeviceId
  , _loginRequest_initialDeviceDisplayName :: Maybe Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON LoginRequest where
  parseJSON = withObject "Login Request" $ \v -> LoginRequest
    <$> v .: "identifier"
    <*> parseLogin v
    <*> v .:? "device_id"
    <*> v .:? "initial_device_display_name"
   where
    parseLogin v = (v .: "type") >>= \case
      Ae.String "m.login.password" -> Login_Password <$> v .: "password"
      _ -> mzero

instance ToJSON LoginRequest where
  toJSON (LoginRequest uid login mdid mdn) = object $ mconcat
    [ pure $ "identifier" .= uid
    , case login of
        Login_Password pw ->
          [ "type" .= Ae.String "m.login.password"
          , "password" .= pw
          ]
    , "device_id" .=? mdid
    , "initial_device_display_name" .=? mdn
    ]

data LoginResponse = LoginResponse
  { _loginResponse_userId :: UserId
  , _loginResponse_accessToken :: AccessToken
  , _loginResponse_homeServer :: Text
  , _loginResponse_deviceId :: DeviceId
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON LoginResponse where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON LoginResponse where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data LogoutRespKey :: Type -> Type where
  LogoutRespKey_200 :: LogoutRespKey LogoutResponse

data LogoutRequest = LogoutRequest
  deriving (Eq, Ord, Show, Generic)

instance FromJSON LogoutRequest where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON LogoutRequest where
  toJSON = genericToJSON aesonOptions

data LogoutResponse = LogoutResponse
  deriving (Eq, Ord, Show, Generic)

instance FromJSON LogoutResponse where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON LogoutResponse where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data LogoutAllRespKey :: Type -> Type where
  LogoutAllRespKey_200 :: LogoutAllRespKey LogoutAllResponse

data LogoutAllRequest = LogoutAllRequest
  deriving (Eq, Ord, Show, Generic)

instance FromJSON LogoutAllRequest where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON LogoutAllRequest where
  toJSON = genericToJSON aesonOptions

data LogoutAllResponse = LogoutAllResponse
  deriving (Eq, Ord, Show, Generic)

instance FromJSON LogoutAllResponse where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON LogoutAllResponse where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data UserIdentifier
  = UserIdentifier_User Text
  -- TODO: Add other User Identifier types
  deriving (Eq, Ord, Show, Generic)

instance FromJSON UserIdentifier where
  parseJSON = withObject "User Identifier" $ \v -> (v .: "type") >>= \case
    Ae.String "m.id.user" -> UserIdentifier_User <$> v .: "user"
    _ -> mzero

instance ToJSON UserIdentifier where
  toJSON = \case
    UserIdentifier_User uid -> object
      [ "type" .= Ae.String "m.id.user"
      , "user" .= Ae.String uid
      ]

data Login
  = Login_Password Text
  -- TODO: Add token login
  deriving (Eq, Ord, Show, Generic)

--------------------------------------------------------------------------------
