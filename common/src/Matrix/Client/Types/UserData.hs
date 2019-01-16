{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}
module Matrix.Client.Types.UserData where

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

data UserDataRoute :: Route where
  UserDataRoute_Search
    :: UserDataRoute
       'POST
       '[ 'Left "user_directory", 'Left "search" ]
       '[ ]
       'True
       UserSearchRequest
       UserSearchRespKey
  UserDataRoute_PutDisplayName
    :: UserDataRoute
       'PUT
       '[ 'Left "profile", 'Right UserId, 'Left "displayname" ]
       '[ ]
       'True
       PutDisplayNameRequest
       PutDisplayNameRespKey
  UserDataRoute_GetDisplayName
    :: UserDataRoute
       'GET
       '[ 'Left "profile", 'Right UserId, 'Left "displayname" ]
       '[ ]
       'False
       GetDisplayNameRequest
       GetDisplayNameRespKey
  UserDataRoute_PutAvatarUrl
    :: UserDataRoute
       'PUT
       '[ 'Left "profile", 'Right UserId, 'Left "avatar_url" ]
       '[ ]
       'True
       PutAvatarUrlRequest
       PutAvatarUrlRespKey
  UserDataRoute_GetAvatarUrl
    :: UserDataRoute
       'GET
       '[ 'Left "profile", 'Right UserId, 'Left "avatar_url" ]
       '[ ]
       'False
       GetAvatarUrlRequest
       GetAvatarUrlRespKey

--------------------------------------------------------------------------------

data UserSearchRespKey :: RespRelation where
  UserSearchRespKey_200 :: UserSearchRespKey 200 UserSearchResponse
  UserSearchRespKey_429 :: UserSearchRespKey 429 Data.Aeson.Value

data UserSearchRequest = UserSearchRequest
  { _userSearchRequest_searchTerm :: Text
  , _userSearchRequest_limit :: Maybe Word32 -- TODO width
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON UserSearchRequest where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON UserSearchRequest where
  toJSON = genericToJSON aesonOptions

data UserSearchResponse = UserSearchResponse
  { _userSearchResponse_results :: [UserSearchResult]
  , _userSearchResponse_limited :: Bool
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON UserSearchResponse where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON UserSearchResponse where
  toJSON = genericToJSON aesonOptions

data UserSearchResult = UserSearchResult
  { _userSearchResult_userId :: UserId
  , _userSearchResult_displayName :: Maybe Text
  , _userSearchResult_avatarUrl :: Maybe MatrixUri
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON UserSearchResult where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON UserSearchResult where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data PutDisplayNameRespKey :: RespRelation where
  PutDisplayNameRespKey_200 :: PutDisplayNameRespKey 200 PutDisplayNameResponse
  PutDisplayNameRespKey_429 :: PutDisplayNameRespKey 429 Data.Aeson.Value

data PutDisplayNameRequest = PutDisplayNameRequest
  { _putDisplayNameRequest_displayname :: Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON PutDisplayNameRequest where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON PutDisplayNameRequest where
  toJSON = genericToJSON aesonOptions

data PutDisplayNameResponse = PutDisplayNameResponse
  deriving (Eq, Ord, Show, Generic)

instance FromJSON PutDisplayNameResponse where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON PutDisplayNameResponse where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data GetDisplayNameRespKey :: RespRelation where
  GetDisplayNameRespKey_200 :: GetDisplayNameRespKey 200 GetDisplayNameResponse
  GetDisplayNameRespKey_404 :: GetDisplayNameRespKey 404 Data.Aeson.Value

data GetDisplayNameRequest = GetDisplayNameRequest
  deriving (Eq, Ord, Show, Generic)

instance FromJSON GetDisplayNameRequest where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON GetDisplayNameRequest where
  toJSON = genericToJSON aesonOptions

data GetDisplayNameResponse = GetDisplayNameResponse
  { _getDisplayNameResponse_displayname :: Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON GetDisplayNameResponse where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON GetDisplayNameResponse where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data PutAvatarUrlRespKey :: RespRelation where
  PutAvatarUrlRespKey_200 :: PutAvatarUrlRespKey 200 PutAvatarUrlResponse
  PutAvatarUrlRespKey_429 :: PutAvatarUrlRespKey 429 Data.Aeson.Value

data PutAvatarUrlRequest = PutAvatarUrlRequest
  { _putAvatarUrlRequest_avatarUrl :: MatrixUri
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON PutAvatarUrlRequest where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON PutAvatarUrlRequest where
  toJSON = genericToJSON aesonOptions

data PutAvatarUrlResponse = PutAvatarUrlResponse
  deriving (Eq, Ord, Show, Generic)

instance FromJSON PutAvatarUrlResponse where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON PutAvatarUrlResponse where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data GetAvatarUrlRespKey :: RespRelation where
  GetAvatarUrlRespKey_200 :: GetAvatarUrlRespKey 200 GetAvatarUrlResponse
  GetAvatarUrlRespKey_404 :: GetAvatarUrlRespKey 404 Data.Aeson.Value

data GetAvatarUrlRequest = GetAvatarUrlRequest
  deriving (Eq, Ord, Show, Generic)

instance FromJSON GetAvatarUrlRequest where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON GetAvatarUrlRequest where
  toJSON = genericToJSON aesonOptions

data GetAvatarUrlResponse = GetAvatarUrlResponse
  { _getAvatarUrlResponse_avatarUrl :: MatrixUri
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON GetAvatarUrlResponse where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON GetAvatarUrlResponse where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

join <$> traverse (deriveArgDict)
  [ ''UserSearchRespKey
  , ''PutDisplayNameRespKey
  ]

join <$> traverse (\ty -> liftA2 (<>) (makeLenses ty) (makeFields ty))
  [ ''UserSearchRequest, ''UserSearchResponse
  , ''PutDisplayNameRequest, ''PutDisplayNameResponse
  ]
