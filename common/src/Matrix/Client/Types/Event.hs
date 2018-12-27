{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
module Matrix.Client.Types.Event where

import           Control.Lens hiding ((.=), (%~))
import           Control.Applicative (liftA2)
import           Control.Monad
import           Data.Aeson
import qualified Data.Aeson as Ae
import           Data.Constraint.Extras.TH
import           Data.GADT.Compare
import           Data.GADT.Compare.TH
import           Data.GADT.Show
import           Data.GADT.Show.TH
import           Data.Constraint
import           Data.Kind
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Some
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Int
import           Data.Word
import           GHC.Generics
import           GHC.TypeLits

import           Data.DependentXhr
import           Matrix.Identifiers
import           Matrix.Client.Types.Common
import           Matrix.Client.Types.Event.Abstract

--------------------------------------------------------------------------------

data EventType :: EventTypeKind where
  -- | Any unknown event type
  EventType_Unknown
    :: [Text]
    -> EventType '(Ae.Object, Ae.Object)
  EventType_Room
    :: RoomEventType tys
    -> EventType tys

deriving instance Eq (EventType a)
deriving instance Ord (EventType a)
deriving instance Show (EventType a)
--deriving instance Generic (EventType a)

instance ToRoutePiece (EventType '(meta, body)) where
  toRoute = \case
    EventType_Unknown txt -> T.intercalate "." txt

instance ToJSON (EventType '(meta, body)) where
  toJSON = Ae.String . toRoute
instance FromJSON (EventType '(meta, body)) where
  parseJSON = undefined -- TOOD

type DefiniteEvent = DefiniteEventAbstract EventType

type Event' = EventAbstract EventType

--------------------------------------------------------------------------------

type Event = Event' Unconstrained

--------------------------------------------------------------------------------

-- | TODO I think this is just opaque string but double check
newtype StateKey = StateKey { getStateKey :: Text }
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

--------------------------------------------------------------------------------

data UnsignedData = UnsignedData
  { _unsignedData_age :: Int64 -- documented to maybe be negative
  , _unsignedData_redactedBecause :: RedactionEvent -- enphasized Optional
  , _unsignedData_transactionId :: Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON UnsignedData where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON UnsignedData where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data StateEventMeta body = StateEventMeta
  { _stateEventMeta_stateKey :: StateKey -- REQUIRED
  , _stateEventMeta_eventId :: EventId -- REQUIRED
  , _stateEventMeta_sender :: Text -- REQUIRED
  , _stateEventMeta_originServerAs :: Int32 -- REQUIRED
  , _stateEventMeta_unsigned :: Maybe UnsignedData -- REQUIRED
  , _stateEventMeta_prevContent :: Maybe body -- REQUIRED
  } deriving (Eq, Ord, Show, Generic)

-- TODO Finish manually expanding to get around staging restriction. See
-- https://ghc.haskell.org/trac/ghc/ticket/12034 for detail.
class HasStateEventMeta body meta
instance HasStateEventMeta body (StateEventMeta body)
-- makeClassy ''StateEventMeta

instance FromJSON body => FromJSON (StateEventMeta body) where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON body => ToJSON (StateEventMeta body) where
  toJSON = genericToJSON aesonOptions

class HasStateEventMeta body meta => IsStateEvent meta body
instance HasStateEventMeta body meta => IsStateEvent meta body

type StateEvent = Event' HasStateEventMeta

--------------------------------------------------------------------------------

data RoomEventMeta = RoomEventMeta
  { _roomEventMeta_eventId :: EventId -- REQUIRED
  , _roomEventMeta_sender :: Text -- REQUIRED
  , _roomEventMeta_unsigned :: UnsignedData -- REQUIRED
  } deriving (Eq, Ord, Show, Generic)

-- TODO finish expanding for reasons described above
class HasRoomEventMeta meta
instance HasRoomEventMeta RoomEventMeta
-- makeClassy ''RoomEventMeta

instance FromJSON RoomEventMeta where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON RoomEventMeta where
  toJSON = genericToJSON aesonOptions

class HasRoomEventMeta meta => IsRoomEvent meta body
instance HasRoomEventMeta meta => IsRoomEvent meta body

type RoomEvent = Event' IsRoomEvent

--------------------------------------------------------------------------------

data RoomEventType :: EventTypeKind where
  -- | m.room.member
  RoomEventType_Member
    :: RoomEventType
       '( MemberEventMeta
        , MemberEventContent)
  -- | m.room.redaction
  RoomEventType_Redaction
    :: RoomEventType
       '( RedactionEventMeta
        , RedactionEventContent)

deriving instance Eq (RoomEventType a)
deriving instance Ord (RoomEventType a)
deriving instance Show (RoomEventType a)
--deriving instance Generic (RoomEventType a)

--------------------------------------------------------------------------------

data MemberEventContent = MemberEventContent
  { _memberEventContent_avatar_url :: MatrixUri
  , _memberEventContent_displayname :: Maybe Text
  , _memberEventContent_membership :: Membership
  , _memberEventContent_isDirect :: Bool
  , _memberEventContent_thirdPartyInvite :: Invite
  , _memberEventContent_unsigned :: UnsignedData
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON MemberEventContent where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON MemberEventContent where
  toJSON = genericToJSON aesonOptions

type MemberEventMeta = (RoomEventMeta, StateEventMeta MemberEventContent)

type MemberEvent = DefiniteEvent MemberEventContent MemberEventMeta

--------------------------------------------------------------------------------

data RedactionEventContent = RedactionEventContent
  { _redactionEventContent_reason :: Text
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON RedactionEventContent where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON RedactionEventContent where
  toJSON = genericToJSON aesonOptions

type RedactionEventMeta = (RoomEventMeta, StateEventMeta RedactionEventContent)

type RedactionEvent = DefiniteEvent RedactionEventContent RedactionEventMeta

--------------------------------------------------------------------------------

-- TODO flesh out
data PowerLevelEventContent = PowerLevelEventContent
 { _powerLevelEventContent_ban :: Maybe Word32 -- TODO width
 } deriving (Eq, Ord, Show, Generic)

instance FromJSON PowerLevelEventContent where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON PowerLevelEventContent where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

data Invite = Invite
  { _invite_displayName :: Text
  , _invite_signed :: Signed
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON Invite where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON Invite where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data Signed = Signed
  { _signed_mxid :: Text
  , _signed_signatures :: Signatures
  , _signed_token :: StateKey
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON Signed where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON Signed where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

-- Definition only found in the server-server spec :/.
newtype Signatures = Signatures (Map Text (Map Text Text))
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

--------------------------------------------------------------------------------

-- TODO but can I properly eliminate this to recover
class (IsRoomEvent meta body, IsStateEvent meta body)
  => IsRoomStateEvent meta body
instance (IsRoomEvent meta body, IsStateEvent meta body)
  => IsRoomStateEvent meta body

type RoomStateEvent = Event' IsRoomStateEvent

--------------------------------------------------------------------------------

join <$> traverse (\ty -> liftA2 (<>) (makeLenses ty) (makeFields ty))
  [ ''UnsignedData
  , ''MemberEventContent
  , ''Invite
  ]

deriveGEq ''EventType
deriveGCompare ''EventType
deriveGShow ''EventType

deriveGEq ''RoomEventType
deriveGCompare ''RoomEventType
deriveGShow ''RoomEventType

--------------------------------------------------------------------------------

-- | The instance for arbitrary 'Some f' because the GADTs JSON forms may not be
-- disjoint.
newtype SomeEventType = SomeEventType (Some EventType)
  deriving (Eq, Ord, Show, Generic)

instance FromJSON SomeEventType where
  parseJSON = undefined -- TODO
instance ToJSON SomeEventType where
  toJSON = undefined -- TODO
