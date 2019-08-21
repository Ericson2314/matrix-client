{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}
module Matrix.Client.Types.Auth.Login where

import           Control.Lens hiding ((.=))
import           Control.Applicative (liftA2)
import           Control.Monad
import           Data.Aeson
import qualified Data.Aeson as Ae
import           Data.Aeson.Utils
import           Data.Constraint.Extras.TH
import           Data.Proxy
import           Data.Some
import           Data.Text (Text)
import           Data.Type.Equality
import           GHC.Generics
import           GHC.TypeLits

import           Data.DependentXhr

import           Matrix.Identifiers
import           Matrix.Client.Types.Common

--------------------------------------------------------------------------------

data LoginRoute :: Route where
  LoginRoute_Login'
    :: LoginRoute
       'GET
       '[ 'Left "login" ]
       '[ ]
       'False
       Login'Request
       Login'RespKey
  LoginRoute_Login
    :: LoginRoute
       'POST
       '[ 'Left "login" ]
       '[ ]
       'False
       LoginRequest
       LoginRespKey
  LoginRoute_Logout
    :: LoginRoute
       'POST
       '[ 'Left "logout" ]
       '[ ]
       'True
       LogoutRequest
       LogoutRespKey
  LoginRoute_LogoutAll
    :: LoginRoute
       'POST
       '[ 'Left "logout", 'Left "all" ]
       '[ ]
       'True
       LogoutAllRequest
       LogoutAllRespKey

--------------------------------------------------------------------------------

data Login'RespKey :: RespRelation where
  Login'RespKey_200 :: Login'RespKey 200 Login'Response
  Login'RespKey_400 :: Login'RespKey 400 Data.Aeson.Value
  Login'RespKey_403 :: Login'RespKey 403 Data.Aeson.Value
  Login'RespKey_429 :: Login'RespKey 429 Data.Aeson.Value

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

data LoginRespKey :: RespRelation where
  LoginRespKey_200 :: LoginRespKey 200 LoginResponse
  LoginRespKey_400 :: LoginRespKey 400 Data.Aeson.Value
  LoginRespKey_403 :: LoginRespKey 403 Data.Aeson.Value
  LoginRespKey_429 :: LoginRespKey 429 Data.Aeson.Value

instance DecidablableLookup LoginRespKey where
  -- TODO handle other cases, make some TH for this.
  liftedLookup
    :: forall status
    .  KnownNat status
    => Decision (Some (LoginRespKey status))
  liftedLookup = case
      sameNat (Proxy :: Proxy status)
              (Proxy :: Proxy 200)
    of
      Just Refl -> Proved $ Some LoginRespKey_200

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

data LogoutRespKey :: RespRelation where
  LogoutRespKey_200 :: LogoutRespKey 200 LogoutResponse

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

data LogoutAllRespKey :: RespRelation where
  LogoutAllRespKey_200 :: LogoutAllRespKey 200 LogoutAllResponse

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

join <$> traverse deriveArgDict
  [ ''Login'RespKey
  , ''LoginRespKey
  , ''LogoutRespKey
  , ''LogoutAllRespKey
  ]

join <$> traverse (\ty -> liftA2 (<>) (makeLenses ty) (makeFields ty))
  [ ''Login'Request, ''Login'Response
  , ''LoginRequest, ''LoginResponse
  , ''LogoutRequest, ''LogoutResponse
  , ''LogoutAllRequest, ''LogoutAllResponse
  ]
