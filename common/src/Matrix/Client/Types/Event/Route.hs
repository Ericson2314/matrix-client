{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}
module Matrix.Client.Types.Event.Route where

import           Control.Lens hiding ((.=))
import           Control.Applicative (liftA2)
import           Control.Monad
import           Data.Aeson
import qualified Data.Aeson as Ae
import           Data.Constraint.Extras.TH
import           Data.Kind
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import           Data.Int
import           Data.Word
import           GHC.Generics

import           Data.DependentXhr
import           Matrix.Identifiers
import           Matrix.Client.Types.Common
import           Matrix.Client.Types.Filter
import           Matrix.Client.Types.Event

--------------------------------------------------------------------------------

data EventRoute :: Route where
  EventRoute_Sync
    :: EventRoute
       'GET
       '[ 'Left "sync"]
       'True
       SyncRequest
       SyncRespKey
  EventRoute_GetRoomEvent
    :: EventRoute
       'GET
       '[ 'Left "rooms", 'Right RoomId, 'Left "event", 'Right EventId ]
       'True
       GetRoomEventRequest
       GetRoomEventRespKey
  EventRoute_GetRoomStateAt
    :: IsRoomStateEvent meta body
    => EventRoute
       'GET
       '[ 'Left "rooms", 'Right RoomId, 'Left "state", 'Right (EventType '(meta, body)), 'Right StateKey ]
       'True
       GetRoomStateRequest
       (GetRoomStateRespKey body)
  EventRoute_GetRoomStateCurrent
    :: IsRoomStateEvent meta body
    => EventRoute
       'GET
       '[ 'Left "rooms", 'Right RoomId, 'Left "state", 'Right (EventType '(meta, body)) ]
       'True
       GetRoomStateRequest
       (GetRoomStateRespKey body)
  EventRoute_GetRoomStateAll
    :: EventRoute
       'GET
       '[ 'Left "rooms", 'Right RoomId, 'Left "state" ]
       'True
       GetRoomStateAllRequest
       GetRoomStateAllRespKey
  EventRoute_GetRoomMembers
    :: EventRoute
       'GET
       '[ 'Left "rooms", 'Right RoomId, 'Left "members" ]
       'True
       GetRoomMembersRequest
       GetRoomMembersRespKey
  EventRoute_GetRoomJoinedMembers
    :: EventRoute
       'GET
       '[ 'Left "rooms", 'Right RoomId, 'Left "joined_members" ]
       'True
       GetRoomJoinedMembersRequest
       GetRoomJoinedMembersRespKey
  EventRoute_GetRoomMessages
    :: EventRoute
       'GET
       '[ 'Left "rooms", 'Right RoomId, 'Left "messages" ]
       'True
       GetRoomMessagesRequest
       GetRoomMessagesRespKey
  EventRoute_PutRoomStateAt
    :: IsRoomStateEvent meta body
    => EventRoute
       'PUT
       '[ 'Left "rooms", 'Right RoomId, 'Left "state", 'Right (EventType '(meta, body)), 'Right StateKey ]
       'True
       () --PutRoomStateAtRequest
       SyncRespKey --PutRoomStateAtRespKey
  EventRoute_PutRoomStateCurrent
    :: IsRoomStateEvent meta body
    => EventRoute
       'PUT
       '[ 'Left "rooms", 'Right RoomId, 'Left "state", 'Right (EventType '(meta, body)) ]
       'True
       () --PutRoomStateCurrentRequest
       SyncRespKey --PutRoomStateCurrentRespKey
  EventRoute_PutRoom
    :: IsRoomEvent meta body
    => EventRoute
       'PUT
       '[ 'Left "rooms", 'Right RoomId, 'Left "send", 'Right (EventType '(meta, body)), 'Right TxnId ]
       'True
       () --PutRoomRequest
       SyncRespKey --PutRoomRespKey
  EventRoute_PutRoomRedact
    :: IsRoomEvent meta body
    => EventRoute
       'PUT
       '[ 'Left "rooms", 'Right RoomId, 'Left "redact", 'Right (EventType '(meta, body)), 'Right TxnId ]
       'True
       () --PutRoomRequestRedact
       SyncRespKey --PutRoomRespKeyRedact

--------------------------------------------------------------------------------

data SyncRespKey :: Type -> Type where
  SyncRespKey_Valid :: SyncRespKey SyncResponse
  SyncRespKey_Invalid :: SyncRespKey Ae.Value

data Filter'
  = Filter'_Id Text
  -- ^ A previously saved filter referenced by an identifier.
  | Filter'_Literal Filter
  -- ^ A previously saved filter referenced by an identifier.
  deriving (Eq, Ord, Show, Generic)

-- TODO fix sum type instances
instance FromJSON Filter' where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON Filter' where
  toJSON = genericToJSON aesonOptions

data ClientPresence
  = ClientPresence_Offline
  | ClientPresence_Online
  | ClientPresence_Unavailable
  deriving (Eq, Ord, Show, Generic, Enum, Bounded)

-- TODO fix enum instances
instance FromJSON ClientPresence where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON ClientPresence where
  toJSON = genericToJSON aesonOptions

-- TODO there might be more structure, but can be used opaquely so plain newtype
-- is probably sufficient.
newtype SyncBatchToken = SyncBatchToken Text
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

data SyncRequest = SyncRequest
  { _syncRequest_filter :: Filter'
  , _syncRequest_since :: SyncBatchToken
  , _syncRequest_fullState :: Bool
  , _syncRequest_setPresence :: ClientPresence
  , _syncRequest_timeout :: Word32 -- TODO width, signedness
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON SyncRequest where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON SyncRequest where
  toJSON = genericToJSON aesonOptions

data SyncResponse = SyncResponse
  { _syncResponse_nextBatch :: SyncBatchToken -- REQUIRED
  , _syncResponse_rooms :: Rooms
  , _syncResponse_presence :: Presence
  , _syncResponse_accountData :: AccountData
  -- , _syncResponse_toDevice :: ToDevice
  -- , _syncResponse_deviceLists :: DeviceLists
  -- , _syncResponse_deviceOneTimeKeysCount :: Map Text Word32
  } deriving ({-Eq, Ord, Show,-} Generic)

instance FromJSON SyncResponse where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON SyncResponse where
  toJSON = genericToJSON aesonOptions

data Rooms = Rooms
  { _rooms_join :: Map Text JoinedRoom
  , _rooms_invite :: Map Text InvitedRoom
  , _rooms_left :: Map Text LeftRoom
  } deriving ({-Eq, Ord, Show,-} Generic)

instance FromJSON Rooms where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON Rooms where
  toJSON = genericToJSON aesonOptions

data JoinedRoom = JoinedRoom
  { _joinedRoom_state :: State
  , _joinedRoom_timeline :: Timeline
  , _joinedRoom_ephemeral :: Ephemeral
  , _joinedRoom_accountData :: AccountData
  , _joinedRoom_unreadNotifications :: UnreadNotificationCounts
  } deriving ({-Eq, Ord, Show,-} Generic)

instance FromJSON JoinedRoom where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON JoinedRoom where
  toJSON = genericToJSON aesonOptions

data Ephemeral = Ephemeral
  { _ephemeral_events :: [Event]
  } deriving ({-Eq, Ord, Show,-} Generic)

instance FromJSON Ephemeral where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON Ephemeral where
  toJSON = genericToJSON aesonOptions

data UnreadNotificationCounts = UnreadNotificationCounts
  { _unreadNotificationCounts_highlightCount :: Word32
  , _unreadNotificationCounts_notificationCount :: Word32
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON UnreadNotificationCounts where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON UnreadNotificationCounts where
  toJSON = genericToJSON aesonOptions

data InvitedRoom = InvitedRoom
  { _InvitedRoom_invite_state :: InviteState
  } deriving ({-Eq, Ord, Show,-} Generic)

instance FromJSON InvitedRoom where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON InvitedRoom where
  toJSON = genericToJSON aesonOptions

data InviteState = InviteState
  { _InviteState_events :: [StrippedState]
  } deriving ({-Eq, Ord, Show,-} Generic)

instance FromJSON InviteState where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON InviteState where
  toJSON = genericToJSON aesonOptions

-- | All fields REQUIRED
data StrippedState = StrippedState
  { _strippedState_content :: () -- TODO work in with Event abstractions
  , _strippedState_state_key :: Text
  , _strippedState_type :: Text
  , _strippedState_sender :: Text
  } deriving ({-Eq, Ord, Show,-} Generic)

instance FromJSON StrippedState where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON StrippedState where
  toJSON = genericToJSON aesonOptions

data LeftRoom = LeftRoom
  { _leftRoom_state :: State
  , _leftRoom_timeline :: Timeline
  , _leftRoom_accountData :: AccountData
  } deriving ({-Eq, Ord, Show,-} Generic)

instance FromJSON LeftRoom where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON LeftRoom where
  toJSON = genericToJSON aesonOptions

data State = State
  { _state_events :: [StateEvent]
  } deriving ({-Eq, Ord, Show,-} Generic)

instance FromJSON State where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON State where
  toJSON = genericToJSON aesonOptions

data Timeline = Timeline
  { _timeline_events :: [RoomEvent]
  , _timeline_limited :: Bool
  , _timeline_prevBatch :: Text
  } deriving ({-Eq, Ord, Show,-} Generic)

instance FromJSON Timeline where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON Timeline where
  toJSON = genericToJSON aesonOptions

data Presence = Presence
  { _presence_events :: [Event]
  } deriving ({-Eq, Ord, Show,-} Generic)

instance FromJSON Presence where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON Presence where
  toJSON = genericToJSON aesonOptions

data AccountData = AccountData
  { _accountData_events :: [Event]
  } deriving ({-Eq, Ord, Show,-} Generic)

instance FromJSON AccountData where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON AccountData where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data GetRoomEventRespKey :: Type -> Type where
  GetRoomEventRespKey_200 :: GetRoomEventRespKey RoomEvent
  GetRoomEventRespKey_404 :: GetRoomEventRespKey Data.Aeson.Value

data GetRoomEventRequest = GetRoomEventRequest
  deriving (Eq, Ord, Show, Generic)

instance FromJSON GetRoomEventRequest where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON GetRoomEventRequest where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data GetRoomStateRespKey :: Type -> Type -> Type where
  GetRoomStateRespKey_200 :: GetRoomStateRespKey body body
  GetRoomStateRespKey_403 :: GetRoomStateRespKey body Data.Aeson.Value
  GetRoomStateRespKey_404 :: GetRoomStateRespKey body Data.Aeson.Value

data GetRoomStateRequest = GetRoomStateRequest
  deriving (Eq, Ord, Show, Generic)

instance FromJSON GetRoomStateRequest where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON GetRoomStateRequest where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data GetRoomStateAllRespKey :: Type -> Type where
  GetRoomStateAllRespKey_200 :: GetRoomStateAllRespKey [RoomStateEvent]
  GetRoomStateAllRespKey_403 :: GetRoomStateAllRespKey Data.Aeson.Value

data GetRoomStateAllRequest = GetRoomStateAllRequest
  deriving (Eq, Ord, Show, Generic)

instance FromJSON GetRoomStateAllRequest where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON GetRoomStateAllRequest where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data GetRoomMembersRespKey :: Type -> Type where
  GetRoomMembersRespKey_200 :: GetRoomMembersRespKey GetRoomMembersResponse
  GetRoomMembersRespKey_403 :: GetRoomMembersRespKey Data.Aeson.Value

data GetRoomMembersRequest = GetRoomMembersRequest
  deriving (Eq, Ord, Show, Generic)

instance FromJSON GetRoomMembersRequest where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON GetRoomMembersRequest where
  toJSON = genericToJSON aesonOptions

data GetRoomMembersResponse = GetRoomMembersResponse
  { _getRoomMembersResponse_chunk :: [MemberEvent]
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON GetRoomMembersResponse where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON GetRoomMembersResponse where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data GetRoomJoinedMembersRespKey :: Type -> Type where
  GetRoomJoinedMembersRespKey_200 :: GetRoomJoinedMembersRespKey GetRoomJoinedMembersResponse
  GetRoomJoinedMembersRespKey_403 :: GetRoomJoinedMembersRespKey Data.Aeson.Value

data GetRoomJoinedMembersRequest = GetRoomJoinedMembersRequest
  deriving (Eq, Ord, Show, Generic)

instance FromJSON GetRoomJoinedMembersRequest where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON GetRoomJoinedMembersRequest where
  toJSON = genericToJSON aesonOptions

data GetRoomJoinedMembersResponse = GetRoomJoinedMembersResponse
  { _getRoomJoinedMembersResponse_joined :: Map UserId RoomMember
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON GetRoomJoinedMembersResponse where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON GetRoomJoinedMembersResponse where
  toJSON = genericToJSON aesonOptions

data RoomMember = RoomMember
  { _roomMember_displayName :: Text
  , _roomMember_avatarUrl :: MatrixUri
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON RoomMember where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON RoomMember where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data GetRoomMessagesRespKey :: Type -> Type where
  GetRoomMessagesRespKey_200 :: GetRoomMessagesRespKey GetRoomMessagesResponse
  GetRoomMessagesRespKey_403 :: GetRoomMessagesRespKey Data.Aeson.Value

data GetRoomMessagesRequest = GetRoomMessagesRequest
  deriving (Eq, Ord, Show, Generic)

instance FromJSON GetRoomMessagesRequest where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON GetRoomMessagesRequest where
  toJSON = genericToJSON aesonOptions

data GetRoomMessagesResponse = GetRoomMessagesResponse
  { _getRoomMessagesResponse_start :: Text
  , _getRoomMessagesResponse_end :: Text
  , _getRoomMessagesResponse_chunk :: [RoomEvent]
  } deriving ({-Eq, Ord, Show,-} Generic)

instance FromJSON GetRoomMessagesResponse where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON GetRoomMessagesResponse where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

join <$> traverse deriveArgDict
  [ ''SyncRespKey
  , ''GetRoomEventRespKey
  -- , ''GetRoomStateRespKey -- TODO scoping but in `deriveArgDict`
  , ''GetRoomStateAllRespKey
  , ''GetRoomMembersRespKey
  , ''GetRoomJoinedMembersRespKey
  ]

join <$> traverse (\ty -> liftA2 (<>) (makeLenses ty) (makeFields ty))
  [ ''SyncRequest, ''SyncResponse
  , ''GetRoomEventRequest
  , ''GetRoomStateRequest
  , ''GetRoomStateAllRequest
  , ''GetRoomMembersRequest
  , ''GetRoomJoinedMembersRequest
  ]
