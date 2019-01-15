{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}
module Matrix.Client.Types.Room where

import           Control.Lens hiding ((.=))
import           Control.Applicative ((<|>), liftA2)
import           Control.Monad
import           Data.Aeson
import           Data.Constraint.Extras.TH
import           Data.Proxy
import           Data.Some
import           Data.Text (Text)
import           Data.Type.Equality
import           Data.Word
import           GHC.Generics
import           GHC.TypeLits

import           Data.DependentXhr

import           Matrix.Identifiers
import           Matrix.Client.Types.Common
import           Matrix.Client.Types.Auth.Account.ThirdParty
import           Matrix.Client.Types.Event

--------------------------------------------------------------------------------

data RoomPreset
  = RoomPreset_PrivateChat
  | RoomPreset_PublicChat
  | RoomPreset_TrustedPrivateChat
  deriving (Eq, Ord, Show, Generic, Enum, Bounded)

-- TODO fix enum instances
instance FromJSON RoomPreset where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON RoomPreset where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data RoomRoute :: Route where
  RoomRoute_CreateRoom
    :: RoomRoute
       'POST
       '[ 'Left "createRoom" ]
       'True
       CreateRoomRequest
       CreateRoomRespKey
  RoomRoute_PutRoomAlias
    :: RoomRoute
       'PUT
       '[ 'Left "directory", 'Left "room", 'Right RoomAlias ]
       'True
       PutRoomAliasRequest
       PutRoomAliasRespKey
  RoomRoute_GetRoomAlias
    :: RoomRoute
       'GET
       '[ 'Left "directory", 'Left "room", 'Right RoomAlias ]
       'False
       GetRoomAliasRequest
       GetRoomAliasRespKey
  RoomRoute_DeleteRoomAlias
    :: RoomRoute
       'DELETE
       '[ 'Left "directory", 'Left "room", 'Right RoomAlias ]
       'True
       DeleteRoomAliasRequest
       DeleteRoomAliasRespKey
  RoomRoute_JoinedRooms
    :: RoomRoute
       'GET
       '[ 'Left "joined_rooms" ]
       'True
       JoinedRoomsRequest
       JoinedRoomsRespKey
  RoomRoute_Invite
    :: RoomRoute
       'PUT
       '[ 'Left "rooms", 'Right RoomId, 'Left "invite" ]
       'True
       InviteRequest
       InviteRespKey
  RoomRoute_Join
    :: RoomRoute
       'PUT
       '[ 'Left "rooms", 'Right RoomId, 'Left "join" ]
       'True
       JoinRequest
       JoinRespKey
  RoomRoute_Join'
    :: RoomRoute
       'PUT
       '[ 'Left "join", 'Right RoomIdOrAlias ]
       'True
       Join'Request
       Join'RespKey
  RoomRoute_Leave
    :: RoomRoute
       'PUT
       '[ 'Left "rooms", 'Right RoomId, 'Left "leave" ]
       'True
       LeaveRequest
       LeaveRespKey
  RoomRoute_Forget
    :: RoomRoute
       'PUT
       '[ 'Left "rooms", 'Right RoomId, 'Left "forget" ]
       'True
       ForgetRequest
       ForgetRespKey
  RoomRoute_Kick
    :: RoomRoute
       'PUT
       '[ 'Left "rooms", 'Right RoomId, 'Left "kick" ]
       'True
       KickRequest
       KickRespKey
  RoomRoute_Ban
    :: RoomRoute
       'PUT
       '[ 'Left "rooms", 'Right RoomId, 'Left "ban" ]
       'True
       BanRequest
       BanRespKey
  RoomRoute_Unban
    :: RoomRoute
       'PUT
       '[ 'Left "rooms", 'Right RoomId, 'Left "unban" ]
       'True
       UnbanRequest
       UnbanRespKey
  RoomRoute_GetRoomListing
    :: RoomRoute
       'GET
       '[ 'Left "directory", 'Left "list", 'Right RoomId ]
       'True
       GetRoomListingRequest
       GetRoomListingRespKey
  RoomRoute_PutRoomListing
    :: RoomRoute
       'PUT
       '[ 'Left "directory", 'Left "list", 'Right RoomId ]
       'True
       PutRoomListingRequest
       PutRoomListingRespKey
  RoomRoute_PublicRooms
    :: RoomRoute
       'GET
       '[ 'Left "publicRooms" ]
       'True
       PublicRoomsRequest
       PublicRoomsRespKey
  RoomRoute_PublicRooms'
    :: RoomRoute
       'POST
       '[ 'Left "publicRooms" ]
       'True
       PublicRooms'Request
       PublicRooms'RespKey

--------------------------------------------------------------------------------

data CreateRoomRespKey :: RespRelation where
  CreateRoomRespKey_200 :: CreateRoomRespKey 200 CreateRoomResponse
  CreateRoomRespKey_400 :: CreateRoomRespKey 400 Data.Aeson.Value

data CreateRoomRequest = CreateRoomRequest
  { _createRoomRequest_visibility :: Visibility
  , _createRoomRequest_roomAliasName :: Maybe RoomName
  , _createRoomRequest_name :: Maybe Text
  , _createRoomRequest_topic :: Maybe Text
  , _createRoomRequest_invite :: [UserId]
  , _createRoomRequest_invite3pid :: [Invite3pid]
  , _createRoomRequest_roomVersion :: Maybe Text -- TODO better type
  , _createRoomRequest_creationContent :: Maybe CreationContent
  , _createRoomRequest_initialState :: [StateEvent]
  , _createRoomRequest_preset :: Maybe RoomPreset
  , _createRoomRequest_isDirect :: Maybe Bool
  , _createRoomRequest_powerLevelContentOverride :: Maybe PowerLevelEventContent
  } deriving ({-Eq, Ord, Show,-} Generic)

instance FromJSON CreateRoomRequest where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON CreateRoomRequest where
  toJSON = genericToJSON aesonOptions

data Invite3pid = Invite3pid
  { _invite3pid_idServer :: ServerName
  , _invite3pid_medium :: ThirdPartyMedium
  , _invite3pid_address :: Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON Invite3pid where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON Invite3pid where
  toJSON = genericToJSON aesonOptions

data CreationContent = CreationContent
  { _createRoomRequest_mFederate :: Maybe Bool
  } deriving (Eq, Ord, Show, Generic)

-- TODO the key name is actually "m.federate".
instance FromJSON CreationContent where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON CreationContent where
  toJSON = genericToJSON aesonOptions

data CreateRoomResponse = CreateRoomResponse
  { _registerResponse_roomId :: RoomId
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON CreateRoomResponse where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON CreateRoomResponse where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data PutRoomAliasRespKey :: RespRelation where
  PutRoomAliasRespKey_200 :: PutRoomAliasRespKey 200 PutRoomAliasResponse
  PutRoomAliasRespKey_409 :: PutRoomAliasRespKey 409 Data.Aeson.Value

data PutRoomAliasRequest = PutRoomAliasRequest
  { _putRoomAliasRequest_roomId :: RoomId
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON PutRoomAliasRequest where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON PutRoomAliasRequest where
  toJSON = genericToJSON aesonOptions

data PutRoomAliasResponse = PutRoomAliasResponse
  deriving (Eq, Ord, Show, Generic)

instance FromJSON PutRoomAliasResponse where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON PutRoomAliasResponse where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data GetRoomAliasRespKey :: RespRelation where
  GetRoomAliasRespKey_200 :: GetRoomAliasRespKey 200 GetRoomAliasResponse
  GetRoomAliasRespKey_409 :: GetRoomAliasRespKey 409 Data.Aeson.Value

data GetRoomAliasRequest = GetRoomAliasRequest
  deriving (Eq, Ord, Show, Generic)

instance FromJSON GetRoomAliasRequest where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON GetRoomAliasRequest where
  toJSON = genericToJSON aesonOptions

data GetRoomAliasResponse = GetRoomAliasResponse
  { _getRoomAliasResponse_roomId :: RoomId
  , _getRoomAliasResponse_servers :: [ServerName]
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON GetRoomAliasResponse where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON GetRoomAliasResponse where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data DeleteRoomAliasRespKey :: RespRelation where
  DeleteRoomAliasRespKey_200 :: DeleteRoomAliasRespKey 200 DeleteRoomAliasResponse

data DeleteRoomAliasRequest = DeleteRoomAliasRequest
  deriving (Eq, Ord, Show, Generic)

instance FromJSON DeleteRoomAliasRequest where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON DeleteRoomAliasRequest where
  toJSON = genericToJSON aesonOptions

data DeleteRoomAliasResponse = DeleteRoomAliasResponse
  deriving (Eq, Ord, Show, Generic)

instance FromJSON DeleteRoomAliasResponse where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON DeleteRoomAliasResponse where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data JoinedRoomsRespKey :: RespRelation where
  JoinedRoomsRespKey_200 :: JoinedRoomsRespKey 200 JoinedRoomsResponse

data JoinedRoomsRequest = JoinedRoomsRequest
  deriving (Eq, Ord, Show, Generic)

instance FromJSON JoinedRoomsRequest where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON JoinedRoomsRequest where
  toJSON = genericToJSON aesonOptions

data JoinedRoomsResponse = JoinedRoomsResponse
  { _joinedRoomsResponse_joinedRooms :: [RoomId]
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON JoinedRoomsResponse where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON JoinedRoomsResponse where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data InviteRespKey :: RespRelation where
  InviteRespKey_200 :: InviteRespKey 200 InviteResponse
  InviteRespKey_403 :: InviteRespKey 403 Data.Aeson.Value
  InviteRespKey_429 :: InviteRespKey 429 Data.Aeson.Value

data InviteRequest = InviteRequest
  { _inviteRequest_userId :: UserId
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON InviteRequest where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON InviteRequest where
  toJSON = genericToJSON aesonOptions

data InviteResponse = InviteResponse
  deriving (Eq, Ord, Show, Generic)

instance FromJSON InviteResponse where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON InviteResponse where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data JoinRespKey :: RespRelation where
  JoinRespKey_200 :: JoinRespKey 200 JoinResponse
  JoinRespKey_403 :: JoinRespKey 403 Data.Aeson.Value
  JoinRespKey_429 :: JoinRespKey 429 Data.Aeson.Value

instance DecidablableLookup JoinRespKey where
  -- TODO handle other cases, make some TH for this.
  liftedLookup
    :: forall status
    .  KnownNat status
    => Decision (Some (JoinRespKey status))
  liftedLookup = case
      sameNat (Proxy :: Proxy status)
              (Proxy :: Proxy 200)
    of
      Just Refl -> Proved $ This JoinRespKey_200

data JoinRequest = JoinRequest
  { _joinRequest_thirdPartySigned :: Maybe ThirdPartySigned
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON JoinRequest where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON JoinRequest where
  toJSON = genericToJSON aesonOptions

data ThirdPartySigned = ThirdPartySigned
  { _thirdPartySigned_sender :: Text
  , _thirdPartySigned_mxid :: Text
  , _thirdPartySigned_token :: StateKey
  , _thirdPartySigned_signatures :: Signatures
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON ThirdPartySigned where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON ThirdPartySigned where
  toJSON = genericToJSON aesonOptions

data JoinResponse = JoinResponse
  { _joinResponse_roomId :: RoomId
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON JoinResponse where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON JoinResponse where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data Join'RespKey :: RespRelation where
  Join'RespKey_200 :: Join'RespKey 200 Join'Response
  Join'RespKey_403 :: Join'RespKey 403 Data.Aeson.Value
  Join'RespKey_429 :: Join'RespKey 429 Data.Aeson.Value

data RoomIdOrAlias
  = RoomIdOrAlias_Id RoomId
  | RoomIdOrAlias_Alias RoomAlias
  deriving (Eq, Ord, Show, Generic)

instance FromJSON RoomIdOrAlias where
  parseJSON v = (RoomIdOrAlias_Id <$> parseJSON v)
          <|> (RoomIdOrAlias_Alias <$> parseJSON v)
instance ToJSON RoomIdOrAlias where
  toJSON = \case
    RoomIdOrAlias_Id rid -> toJSON rid
    RoomIdOrAlias_Alias ralias -> toJSON ralias

data Join'Request = Join'Request
  { _join'Request_thirdPartySigned :: Maybe ThirdPartySigned
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON Join'Request where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON Join'Request where
  toJSON = genericToJSON aesonOptions

data Join'Response = Join'Response
  { _join'Response_roomId :: RoomId
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON Join'Response where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON Join'Response where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data LeaveRespKey :: RespRelation where
  LeaveRespKey_200 :: LeaveRespKey 200 LeaveResponse

data LeaveRequest = LeaveRequest
  deriving (Eq, Ord, Show, Generic)

instance FromJSON LeaveRequest where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON LeaveRequest where
  toJSON = genericToJSON aesonOptions

data LeaveResponse = LeaveResponse
  deriving (Eq, Ord, Show, Generic)

instance FromJSON LeaveResponse where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON LeaveResponse where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data ForgetRespKey :: RespRelation where
  ForgetRespKey_200 :: ForgetRespKey 200 ForgetResponse
  ForgetRespKey_429 :: ForgetRespKey 429 Data.Aeson.Value

data ForgetRequest = ForgetRequest
  deriving (Eq, Ord, Show, Generic)

instance FromJSON ForgetRequest where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON ForgetRequest where
  toJSON = genericToJSON aesonOptions

data ForgetResponse = ForgetResponse
  deriving (Eq, Ord, Show, Generic)

instance FromJSON ForgetResponse where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON ForgetResponse where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data KickRespKey :: RespRelation where
  KickRespKey_200 :: KickRespKey 200 KickResponse
  KickRespKey_403 :: KickRespKey 403 Data.Aeson.Value

data KickRequest = KickRequest
  { _kickRequest_userId :: UserId
  , _kickRequest_reason :: Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON KickRequest where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON KickRequest where
  toJSON = genericToJSON aesonOptions

data KickResponse = KickResponse
  deriving (Eq, Ord, Show, Generic)

instance FromJSON KickResponse where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON KickResponse where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data BanRespKey :: RespRelation where
  BanRespKey_200 :: BanRespKey 200 BanResponse
  BanRespKey_403 :: BanRespKey 403 Data.Aeson.Value

data BanRequest = BanRequest
  { _banRequest_userId :: UserId
  , _banRequest_reason :: Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON BanRequest where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON BanRequest where
  toJSON = genericToJSON aesonOptions

data BanResponse = BanResponse
  deriving (Eq, Ord, Show, Generic)

instance FromJSON BanResponse where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON BanResponse where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data UnbanRespKey :: RespRelation where
  UnbanRespKey_200 :: UnbanRespKey 200 UnbanResponse
  UnbanRespKey_403 :: UnbanRespKey 403 Data.Aeson.Value

data UnbanRequest = UnbanRequest
  { _unbanRequest_userId :: UserId
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON UnbanRequest where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON UnbanRequest where
  toJSON = genericToJSON aesonOptions

data UnbanResponse = UnbanResponse
  deriving (Eq, Ord, Show, Generic)

instance FromJSON UnbanResponse where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON UnbanResponse where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data GetRoomListingRespKey :: RespRelation where
  GetRoomListingRespKey_200 :: GetRoomListingRespKey 200 GetRoomListingResponse
  GetRoomListingRespKey_404 :: GetRoomListingRespKey 404 Data.Aeson.Value

data GetRoomListingRequest = GetRoomListingRequest
  deriving (Eq, Ord, Show, Generic)

instance FromJSON GetRoomListingRequest where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON GetRoomListingRequest where
  toJSON = genericToJSON aesonOptions

data GetRoomListingResponse = GetRoomListingResponse
  { _getRoomListingResponse_visibility :: Visibility
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON GetRoomListingResponse where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON GetRoomListingResponse where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data PutRoomListingRespKey :: RespRelation where
  PutRoomListingRespKey_200 :: PutRoomListingRespKey 200 PutRoomListingResponse
  PutRoomListingRespKey_404 :: PutRoomListingRespKey 404 Data.Aeson.Value

data PutRoomListingRequest = PutRoomListingRequest
  { _putRoomListingRequest_visibility :: Visibility
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON PutRoomListingRequest where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON PutRoomListingRequest where
  toJSON = genericToJSON aesonOptions

data PutRoomListingResponse = PutRoomListingResponse
  deriving (Eq, Ord, Show, Generic)

instance FromJSON PutRoomListingResponse where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON PutRoomListingResponse where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data PublicRoomsRespKey :: RespRelation where
  PublicRoomsRespKey_200 :: PublicRoomsRespKey 200 PublicRoomsResponse

data PublicRoomsRequest = PublicRoomsRequest
  deriving (Eq, Ord, Show, Generic)

instance FromJSON PublicRoomsRequest where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON PublicRoomsRequest where
  toJSON = genericToJSON aesonOptions

data PublicRoomsResponse = PublicRoomsResponse
  { _publicRoomsResponse_chunk :: [PublicRoomInfo]
  , _publicRoomsResponse_nextBatch :: Maybe Text
  , _publicRoomsResponse_prevBatch :: Maybe Text
  , _publicRoomsResponse_totalRoomCountEstimate :: Maybe Word32
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON PublicRoomsResponse where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON PublicRoomsResponse where
  toJSON = genericToJSON aesonOptions

data PublicRoomInfo = PublicRoomInfo
  { _publicRoomInfo_aliases :: [RoomAlias]
  , _publicRoomInfo_cannonicalAlias :: Maybe RoomAlias
  , _publicRoomInfo_name :: Maybe RoomName
  , _publicRoomInfo_numJoinedMembers :: Word32 -- TODO width
  , _publicRoomInfo_roomId :: RoomId
  , _publicRoomInfo_topic :: Maybe Text
  , _publicRoomInfo_worldReadable :: Bool
  , _publicRoomInfo_guestCanJoin :: Bool
  , _publicRoomInfo_avatarUrl :: MatrixUri
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON PublicRoomInfo where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON PublicRoomInfo where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data PublicRooms'RespKey :: RespRelation where
  PublicRooms'RespKey_200 :: PublicRooms'RespKey 200 PublicRooms'Response

data PublicRooms'Request = PublicRooms'Request
  { _publicRooms'Request_server :: [ServerName]
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON PublicRooms'Request where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON PublicRooms'Request where
  toJSON = genericToJSON aesonOptions

data PublicRooms'Response = PublicRooms'Response
  { _publicRooms'Response_chunk :: [PublicRoomInfo]
  , _publicRooms'Response_nextBatch :: Maybe Text
  , _publicRooms'Response_prevBatch :: Maybe Text
  , _publicRooms'Response_totalRoomCountEstimate :: Maybe Word32
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON PublicRooms'Response where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON PublicRooms'Response where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

join <$> traverse (deriveArgDict)
  [ ''CreateRoomRespKey
  , ''PutRoomAliasRespKey
  , ''DeleteRoomAliasRespKey
  , ''GetRoomAliasRespKey
  , ''JoinedRoomsRespKey
  , ''InviteRespKey
  , ''JoinRespKey
  , ''Join'RespKey
  , ''LeaveRespKey
  , ''ForgetRespKey
  , ''KickRespKey
  , ''BanRespKey
  , ''UnbanRespKey
  , ''GetRoomListingRespKey
  , ''PutRoomListingRespKey
  , ''PublicRoomsRespKey
  , ''PublicRooms'RespKey
  ]

join <$> traverse (\ty -> liftA2 (<>) (makeLenses ty) (makeFields ty))
  [ ''CreateRoomRequest, ''CreateRoomResponse
  , ''PutRoomAliasRequest, ''PutRoomAliasResponse
  , ''DeleteRoomAliasRequest, ''DeleteRoomAliasResponse
  , ''GetRoomAliasRequest, ''GetRoomAliasResponse
  , ''JoinedRoomsRequest, ''JoinedRoomsResponse
  , ''InviteRequest, ''InviteResponse
  , ''JoinRequest, ''JoinResponse
  , ''Join'Request, ''Join'Response
  , ''LeaveRequest, ''LeaveResponse
  , ''ForgetRequest, ''ForgetResponse
  , ''KickRequest, ''KickResponse
  , ''BanRequest, ''BanResponse
  , ''UnbanRequest, ''UnbanResponse
  , ''GetRoomListingRequest, ''GetRoomListingResponse
  , ''PutRoomListingRequest, ''PutRoomListingResponse
  , ''PublicRoomsRequest, ''PublicRoomsResponse
  , ''PublicRooms'Request, ''PublicRooms'Response
  ]
