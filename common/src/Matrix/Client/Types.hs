{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Matrix.Client.Types where

import Control.Monad
import Data.Aeson
import Data.Aeson.Utils
import Data.Text (Text)

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
  deriving (Eq, Ord, Show, FromJSON, ToJSON)
