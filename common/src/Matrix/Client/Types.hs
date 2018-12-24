{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}
module Matrix.Client.Types
  ( module Matrix.Client.Types
  , module Matrix.Client.Types.Auth.Login
  , module Matrix.Client.Types.Auth.Account
  ) where

import           Control.Lens hiding ((.=))
import           Control.Applicative (liftA2)
import           Control.Monad
import           Data.Aeson
import qualified Data.Aeson as Ae
import           Data.Constraint.Extras.TH
import           Data.DependentXhr
import           Data.Kind
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Some
import           Data.Text (Text)
import           Data.Int
import           Data.Word
import           GHC.Generics

import           Matrix.Identifiers
import           Matrix.Client.Types.Common
import           Matrix.Client.Types.Auth.Login
import           Matrix.Client.Types.Auth.Account

--------------------------------------------------------------------------------

type Prefix r = 'Left "_matrix" ': 'Left "client" ': 'Left "r0" ': r

-- | The Matrix interface for the client to talk to the surver.
data ClientServerRoute :: Route where
  ClientServerRoute_Login
    :: LoginRoute httpType route needsAuth request respPerCode
    -> ClientServerRoute
       httpType
       (Prefix route)
       needsAuth
       request
       respPerCode
  ClientServerRoute_Account
    :: AccountRoute httpType route needsAuth request respPerCode
    -> ClientServerRoute
       httpType
       (Prefix route)
       needsAuth
       request
       respPerCode
  ClientServerRoute_Sync
    :: ClientServerRoute
       "GET"
       ['Left "_matrix", 'Left "client", 'Left "r0", 'Left "sync"]
       'True
       SyncRequest
       SyncRespKey
  ClientServerRoute_PutFilter
    :: ClientServerRoute
       "POST"
       ['Left "_matrix", 'Left "client", 'Left "r0", 'Left "user", 'Right UserId, 'Left "filter"]
       'True
       Ae.Value
       PutFilterRespKey
  ClientServerRoute_Join
    :: ClientServerRoute
       "PUT"
       ['Left "_matrix", 'Left "client", 'Left "r0", 'Left "rooms", 'Right RoomId, 'Left "join"]
       'True
       JoinRequest
       JoinRespKey

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
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON SyncResponse where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON SyncResponse where
  toJSON = genericToJSON aesonOptions

data Rooms = Rooms
  { _rooms_join :: Map Text JoinedRoom
  , _rooms_invite :: Map Text InvitedRoom
  , _rooms_left :: Map Text LeftRoom
  } deriving (Eq, Ord, Show, Generic)

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
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON JoinedRoom where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON JoinedRoom where
  toJSON = genericToJSON aesonOptions

data Ephemeral = Ephemeral
  { _ephemeral_events :: [Event ()] -- TODO fix `()`
  } deriving (Eq, Ord, Show, Generic)

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
  { _InvitedRoom_invite_state :: InviteState -- [Event]
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON InvitedRoom where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON InvitedRoom where
  toJSON = genericToJSON aesonOptions

data InviteState = InviteState
  { _InviteState_events :: [StrippedState]
  } deriving (Eq, Ord, Show, Generic)

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
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON StrippedState where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON StrippedState where
  toJSON = genericToJSON aesonOptions

data EventContent = EventContent
  { _eventContent_avatar_url :: MatrixUri
  , _eventContent_displayname :: Maybe Text
  , _eventContent_membership :: Membership
  , _eventContent_isDirect :: Bool
  , _eventContent_thirdPartyInvite :: Invite
  , _eventContent_unsigned ::UnsignedData
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON EventContent where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON EventContent where
  toJSON = genericToJSON aesonOptions

data Invite = Invite
  { _invite_displayName :: Text
  , _invite_signed :: Signed
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON Invite where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON Invite where
  toJSON = genericToJSON aesonOptions

data Signed = Signed
  { _signed_mxid :: Text
  , _signed_signatures :: Signatures
  , _signed_token :: Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON Signed where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON Signed where
  toJSON = genericToJSON aesonOptions

-- Definition only found in the server-server spec :/.
newtype Signatures = Signatures (Map Text (Map Text Text))
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

data Membership
  = Membership_Invite
  | Membership_Join
  | Membership_Knock
  | Membership_Leave
  | Membership_Ban
  deriving (Eq, Ord, Show, Generic, Enum, Bounded)

-- TODO fix enum instances
instance FromJSON Membership where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON Membership where
  toJSON = genericToJSON aesonOptions

data LeftRoom = LeftRoom
  { _leftRoom_state :: State
  , _leftRoom_timeline :: Timeline
  , _leftRoom_accountData :: AccountData
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON LeftRoom where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON LeftRoom where
  toJSON = genericToJSON aesonOptions

data State = State
  { _state_events :: [StateEvent ()] -- TODO fix `()`
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON State where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON State where
  toJSON = genericToJSON aesonOptions

-- | Added C parameter so as to keep having Ord instances.
data StateEvent c = StateEvent
  { _stateEvent_content :: c -- REQUIRED
  , _stateEvent_type :: Text -- REQUIRED
  , _stateEvent_eventId :: Text -- REQUIRED
  , _stateEvent_sender :: Text -- REQUIRED
  , _stateEvent_originServerAs :: Int32 -- REQUIRED
  , _stateEvent_unsigned :: UnsignedData
  , _stateEvent_prevContent :: EventContent
  , _stateEvent_stateKey :: EventContent -- REQUIRED
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON c => FromJSON (StateEvent c) where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON c => ToJSON (StateEvent c) where
  toJSON = genericToJSON aesonOptions

data Timeline = Timeline
  { _timeline_events :: [RoomEvent ()] -- TODO fix `()`
  , _timeline_limited :: Bool
  , _timeline_prevBatch :: Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON Timeline where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON Timeline where
  toJSON = genericToJSON aesonOptions

data RoomEvent c = RoomEvent
  { _roomEvent_content :: c -- REQUIRED
  , _roomEvent_type :: Text -- REQUIRED
  , _roomEvent_eventId :: Text -- REQUIRED
  , _roomEvent_sender :: Text -- REQUIRED
  , _roomEvent_unsigned :: UnsignedData -- REQUIRED
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON c => FromJSON (RoomEvent c) where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON c => ToJSON (RoomEvent c) where
  toJSON = genericToJSON aesonOptions

data UnsignedData = UnsignedData
  { _unsignedData_age :: Int64 -- documented to maybe be negative
  , _unsignedData_redactedBecause :: Event () -- enphasized Optional
  , _unsignedData_transactionId :: Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON UnsignedData where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON UnsignedData where
  toJSON = genericToJSON aesonOptions

data Presence = Presence
  { _presence_events :: [Event ()] -- TODO fix `()`
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON Presence where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON Presence where
  toJSON = genericToJSON aesonOptions

data AccountData = AccountData
  { _accountData_events :: [Event ()] -- TODO fix `()`
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON AccountData where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON AccountData where
  toJSON = genericToJSON aesonOptions

data Event c = Event
  { _content :: c
  , _type :: Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON c => FromJSON (Event c) where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON c => ToJSON (Event c) where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data PutFilterRespKey :: Type -> Type where
  PutFilterRespKey_Valid :: PutFilterRespKey PutFilterResponse
  PutFilterRespKey_Invalid :: PutFilterRespKey Ae.Value

data PutFilterResponse = PutFilterResponse {}
  deriving (Eq, Ord, Show, Generic)

data Filter = Filter ()
  deriving (Eq, Ord, Show, Generic)

instance FromJSON Filter where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON Filter where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data JoinRespKey :: Type -> Type where
  JoinRespKey_200 :: JoinRespKey JoinResponse
  JoinRespKey_403 :: JoinRespKey Ae.Value
  JoinRespKey_429 :: JoinRespKey Ae.Value

instance GetStatusKey JoinRespKey where
  statusMap = Map.fromList
    [ (200, This JoinRespKey_200)
    , (403, This JoinRespKey_403)
    , (429, This JoinRespKey_429)
    ]

data JoinRequest = JoinRequest
  { -- _joinResponse_thirdPartyId :: ThirdPartyId
  }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON JoinRequest where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON JoinRequest where
  toJSON = genericToJSON aesonOptions

data JoinResponse = JoinResponse
  { _joinResponse_roomId :: RoomId
  }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON JoinResponse where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON JoinResponse where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

join <$> traverse deriveArgDict
  [ ''SyncRespKey
  , ''JoinRespKey
  ]

join <$> traverse (\ty -> liftA2 (<>) (makeLenses ty) (makeFields ty))
  [ ''SyncRequest, ''SyncResponse
  ]
