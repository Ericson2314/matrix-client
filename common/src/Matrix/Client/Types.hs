{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Matrix.Client.Types where

import Control.Lens (makeLenses)
import Control.Monad
import Data.Aeson
import Data.Aeson.Utils
import Data.Constraint.Extras.TH
import Data.DependentXhr
import Data.Kind
import Data.Some
import Data.Text (Text)
import GHC.Generics

import Matrix.Identifiers

-- | The Matrix interface for the client to talk to the surver.
data ClientServer httpType route request respPerCode where
  ClientServer_Login
    :: ClientServer "POST" "/_matrix/client/r0/login" LoginRequest LoginRespKey

data LoginRespKey :: Type -> Type where
  LoginRespKey_Valid :: LoginRespKey LoginResponse
  LoginRespKey_Invalid :: LoginRespKey Data.Aeson.Value

instance GetStatusKey LoginRespKey where
  lookupStatus status
    | status >= 200 && status < 400 = Right $ This LoginRespKey_Valid
    | otherwise = Right $ This LoginRespKey_Invalid

data LoginRequest = LoginRequest
  { _loginRequest_identifier :: UserIdentifier
  , _loginRequest_login :: Login
  , _loginRequest_deviceId :: Maybe DeviceId
  , _loginRequest_initialDeviceDisplayName :: Maybe Text
  } deriving (Eq, Ord, Show)

instance FromJSON LoginRequest where
  parseJSON = withObject "Login Request" $ \v -> LoginRequest
    <$> v .: "identifier"
    <*> parseLogin v
    <*> v .:? "device_id"
    <*> v .:? "initial_device_display_name"
   where
    parseLogin v = (v .: "type") >>= \case
      String "m.login.password" -> Login_Password <$> v .: "password"
      _ -> mzero

instance ToJSON LoginRequest where
  toJSON (LoginRequest uid login mdid mdn) = object $ mconcat
    [ pure $ "identifier" .= uid
    , case login of
        Login_Password pw ->
          [ "type" .= String "m.login.password"
          , "password" .= pw
          ]
    , "device_id" .=? mdid
    , "initial_device_display_name" .=? mdn
    ]

data UserIdentifier
  = UserIdentifier_User Text
  -- TODO: Add other User Identifier types
  deriving (Eq, Ord, Show)

instance FromJSON UserIdentifier where
  parseJSON = withObject "User Identifier" $ \v -> (v .: "type") >>= \case
    String "m.id.user" -> UserIdentifier_User <$> v .: "user"
    _ -> mzero

instance ToJSON UserIdentifier where
  toJSON = \case
    UserIdentifier_User uid -> object
      [ "type" .= String "m.id.user"
      , "user" .= String uid
      ]

data Login
  = Login_Password Text
  -- TODO: Add token login
  deriving (Eq, Ord, Show)

newtype DeviceId = DeviceId { unDeviceId :: Text }
  deriving (Eq, Ord, Show)
  deriving newtype (FromJSON, ToJSON)

data LoginResponse = LoginResponse
  { _loginResponse_userId :: UserId
  , _loginResponse_accessToken :: Text
  , _loginResponse_homeServer :: Text
  , _loginResponse_deviceId :: DeviceId
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON LoginResponse where
  parseJSON = genericParseJSON options

instance ToJSON LoginResponse where
  toJSON = genericToJSON options

options :: Options
options = defaultOptions { fieldLabelModifier = unCamelPrefixedField }

-- | Strips a field name of its type name and separating underscore (optionally
-- preceeded by an initial underscore as well) and converts CamelCase to
-- lowercase_with_underscores.
unCamelPrefixedField :: String -> String
unCamelPrefixedField =
  camelTo2 '_' . tail . dropWhile (/= '_') . dropWhile (== '_')

makeLenses ''LoginResponse
makeLenses ''LoginRequest
deriveArgDict ''LoginRespKey
