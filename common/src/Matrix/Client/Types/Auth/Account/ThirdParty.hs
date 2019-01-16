{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}
module Matrix.Client.Types.Auth.Account.ThirdParty where

import           Control.Lens hiding ((.=))
import           Control.Applicative (liftA2)
import           Control.Monad
import           Data.Aeson
import           Data.Constraint.Extras.TH
import           Data.Text (Text)
import           Data.Word
import           GHC.Generics

import           Data.DependentXhr
import           Matrix.Identifiers
import           Matrix.Client.Types.Common

--------------------------------------------------------------------------------

data Account3PIdRoute :: Route where
  Account3PIdRoute_Get
    :: Account3PIdRoute
       'GET
       '[ ]
       '[ ]
       'True
       Get3PIdRequest
       Get3PIdRespKey
  Account3PIdRoute_Add
    :: Account3PIdRoute
       'POST
       '[ ]
       '[ ]
       'True
       Add3PIdRequest
       Add3PIdRespKey
  Account3PIdRoute_Delete
    :: Account3PIdRoute
       'POST
       '[ 'Left "delete" ]
       '[ ]
       'True
       Delete3PIdRequest
       Delete3PIdRespKey
  Account3PIdRoute_EmailRequestToken
    :: Account3PIdRoute
       'POST
       '[ 'Left "email", 'Left "requestToken" ]
       '[ ]
       'False
       EmailRequestTokenRequest
       EmailRequestTokenRespKey
  Account3PIdRoute_PhoneEmailRequestToken
    :: Account3PIdRoute
       'POST
       '[ 'Left "msisdn", 'Left "requestToken" ]
       '[ ]
       'False
       PhoneRequestTokenRequest
       PhoneRequestTokenRespKey

--------------------------------------------------------------------------------

data ThirdPartyIdentifier = ThirdPartyIdentifier
  { _thirdPartyIdentifier_medium :: ThirdPartyMedium
  , _thirdPartyIdentifier_address :: Text
  , _thirdPartyIdentifier_validatedAt :: Word32
  , _thirdPartyIdentifier_addedAt :: Word32
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON ThirdPartyIdentifier where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON ThirdPartyIdentifier where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

-- TODO make GADT to enforce the address fields being of the right type.
data ThirdPartyMedium
  = ThirdPartyMedium_Email
  | ThirdPartyMedium_msisdn
  deriving (Eq, Ord, Show, Generic, Enum, Bounded)

-- TODO fix enum instances
instance FromJSON ThirdPartyMedium where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON ThirdPartyMedium where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

-- | Don't really like the name/capitalization, but that's what the spec has.
data ThreePidCredentials = ThreePidCredentials
  { _threePidCredentials_clientSecret :: Text
  , _threePidCredentials_idServer :: ServerName
  , _threePidCredentials_sid :: Sid
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON ThreePidCredentials where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON ThreePidCredentials where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

-- TODO regex and max len
newtype Sid = Sid { getSid :: Text }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON Sid where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON Sid where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data Get3PIdRespKey :: RespRelation where
  Get3PIdRespKey_200 :: Get3PIdRespKey 200 Get3PIdResponse

data Get3PIdRequest = Get3PIdRequest
  deriving (Eq, Ord, Show, Generic)

instance FromJSON Get3PIdRequest where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON Get3PIdRequest where
  toJSON = genericToJSON aesonOptions

data Get3PIdResponse = Get3PIdResponse
  { _get3PIdResponse_threepids :: [ThirdPartyIdentifier]
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON Get3PIdResponse where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON Get3PIdResponse where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data Add3PIdRespKey :: RespRelation where
  Add3PIdRespKey_200 :: Add3PIdRespKey 200 Add3PIdResponse
  Add3PIdRespKey_403 :: Add3PIdRespKey 403 Data.Aeson.Value

data Add3PIdRequest = Add3PIdRequest
  { _add3PIdRequest_threePidCreds :: ThreePidCredentials
  , _add3PIdRequest_bind :: Maybe Bool
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON Add3PIdRequest where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON Add3PIdRequest where
  toJSON = genericToJSON aesonOptions

data Add3PIdResponse = Add3PIdResponse
  deriving (Eq, Ord, Show, Generic)

instance FromJSON Add3PIdResponse where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON Add3PIdResponse where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data Delete3PIdRespKey :: RespRelation where
  Delete3PiRespKey_200 :: Delete3PIdRespKey 200 Delete3PIdResponse

data Delete3PIdRequest = Delete3PIdRequest
  { _delete3PIdRequest_medium :: ThirdPartyMedium
  , _delete3PIdRequest_address :: Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON Delete3PIdRequest where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON Delete3PIdRequest where
  toJSON = genericToJSON aesonOptions

data Delete3PIdResponse = Delete3PIdResponse
  deriving (Eq, Ord, Show, Generic)

instance FromJSON Delete3PIdResponse where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON Delete3PIdResponse where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data EmailRequestTokenRespKey :: RespRelation where
  EmailRequestTokenRespKey_200 :: EmailRequestTokenRespKey 200 EmailRequestTokenResponse
  EmailRequestTokenRespKey_403 :: EmailRequestTokenRespKey 403 Data.Aeson.Value

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
  { _emailRequestTokenResponse_sid :: Sid
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON EmailRequestTokenResponse where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON EmailRequestTokenResponse where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data PhoneRequestTokenRespKey :: RespRelation where
  PhoneRequestTokenRespKey_200 :: PhoneRequestTokenRespKey 200 PhoneRequestTokenResponse
  PhoneRequestTokenRespKey_403 :: PhoneRequestTokenRespKey 403 Data.Aeson.Value

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
  { _phoneRequestTokenResponse_sid :: Sid
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON PhoneRequestTokenResponse where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON PhoneRequestTokenResponse where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

join <$> traverse (deriveArgDict)
  [ ''Get3PIdRespKey
  , ''Add3PIdRespKey
  , ''Delete3PIdRespKey
  , ''EmailRequestTokenRespKey
  , ''PhoneRequestTokenRespKey
  ]

join <$> traverse (\ty -> liftA2 (<>) (makeLenses ty) (makeFields ty))
  [ ''Get3PIdRequest, ''Get3PIdResponse
  , ''Add3PIdRequest, ''Add3PIdResponse
  , ''Delete3PIdRequest, ''Delete3PIdResponse
  , ''EmailRequestTokenRequest, ''EmailRequestTokenResponse
  , ''PhoneRequestTokenRequest, ''PhoneRequestTokenResponse
  ]
