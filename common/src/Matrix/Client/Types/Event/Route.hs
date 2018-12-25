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
       () --GetRoomEventRequest
       SyncRespKey --GetRoomEventRespKey
  EventRoute_GetRoomStateAt
    :: (HasRoomEventMeta meta, HasStateEventMeta meta)
    => EventRoute
       'GET
       '[ 'Left "rooms", 'Right RoomId, 'Left "state", 'Right (EventType '(meta, body)), 'Right StateKey ]
       'True
       body --GetRoomStateAtRequest
       SyncRespKey --GetRoomStateAtRespKey
  EventRoute_GetRoomStateCurrent
    :: (HasRoomEventMeta meta, HasStateEventMeta meta)
    => EventRoute
       'GET
       '[ 'Left "rooms", 'Right RoomId, 'Left "state", 'Right (EventType '(meta, body)) ]
       'True
       body --GetRoomStateCurrentRequest
       SyncRespKey --GetRoomStateCurrentRespKey
  EventRoute_GetRoomStateAll
    :: EventRoute
       'GET
       '[ 'Left "rooms", 'Right RoomId, 'Left "state" ]
       'True
       () --GetRoomStateAllRequest
       SyncRespKey --GetRoomStateAllRespKey
  EventRoute_GetRoomMembers
    :: EventRoute
       'GET
       '[ 'Left "rooms", 'Right RoomId, 'Left "members" ]
       'True
       () --GetRoomMembersRequest
       SyncRespKey --GetRoomMembersRespKey
  EventRoute_GetRoomJoinedMembers
    :: EventRoute
       'GET
       '[ 'Left "rooms", 'Right RoomId, 'Left "joined_members" ]
       'True
       () --GetRoomJoinedMembersRequest
       SyncRespKey --GetRoomJoinedMembersRespKey
  EventRoute_GetRoomMessages
    :: EventRoute
       'GET
       '[ 'Left "rooms", 'Right RoomId, 'Left "messages" ]
       'True
       () --GetRoomMessagesRequest
       SyncRespKey --GetRoomMessagesRespKey
  EventRoute_PutRoomStateAt
    :: (HasRoomEventMeta meta, HasStateEventMeta meta)
    => EventRoute
       'PUT
       '[ 'Left "rooms", 'Right RoomId, 'Left "state", 'Right (EventType '(meta, body)), 'Right StateKey ]
       'True
       () --PutRoomStateAtRequest
       SyncRespKey --PutRoomStateAtRespKey
  EventRoute_PutRoomStateCurrent
    :: (HasRoomEventMeta meta, HasStateEventMeta meta)
    => EventRoute
       'PUT
       '[ 'Left "rooms", 'Right RoomId, 'Left "state", 'Right (EventType '(meta, body)) ]
       'True
       () --PutRoomStateCurrentRequest
       SyncRespKey --PutRoomStateCurrentRespKey
  EventRoute_PutRoom
    :: HasRoomEventMeta meta
    => EventRoute
       'PUT
       '[ 'Left "rooms", 'Right RoomId, 'Left "send", 'Right (EventType '(meta, body)), 'Right TxnId ]
       'True
       () --PutRoomRequest
       SyncRespKey --PutRoomRespKey
  EventRoute_PutRoomRedact
    :: HasRoomEventMeta meta
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
  { _strippedState_content :: EventContent
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

join <$> traverse deriveArgDict
  [ ''SyncRespKey
  ]

join <$> traverse (\ty -> liftA2 (<>) (makeLenses ty) (makeFields ty))
  [ ''SyncRequest, ''SyncResponse
  ]
