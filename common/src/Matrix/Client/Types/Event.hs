{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
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

--------------------------------------------------------------------------------

data EventType :: (Type, Type) -> Type where
  EventType_Unknown :: [Text] -> EventType '(Ae.Object, Ae.Object)
  --deriving (Eq, Ord, Show, Generic)

deriveGEq ''EventType
deriveGCompare ''EventType
deriveGShow ''EventType

instance ToRoutePiece (EventType '(meta, body)) where
  toRoute = \case
    EventType_Unknown txt -> T.intercalate "." txt

instance ToJSON (EventType '(meta, body)) where
  toJSON = Ae.String . toRoute
instance FromJSON (EventType '(meta, body)) where
  parseJSON = undefined -- TOOD

--------------------------------------------------------------------------------

-- | The instance for arbitrary 'Some f' because the GADTs JSON forms may not be
-- disjoint.
newtype SomeEventType = SomeEventType (Some EventType)
  deriving (Eq, Ord, Show, Generic)

instance FromJSON SomeEventType where
  parseJSON = undefined -- TODO
instance ToJSON SomeEventType where
  toJSON = undefined -- TODO

--------------------------------------------------------------------------------

-- | TODO I think this is just opaque string but double check
newtype StateKey = StateKey { getStateKey :: Text }
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

--------------------------------------------------------------------------------

data UnsignedData = UnsignedData
  { _unsignedData_age :: Int64 -- documented to maybe be negative
  , _unsignedData_redactedBecause :: Event -- enphasized Optional
  , _unsignedData_transactionId :: Text
  } deriving ({-Eq, Ord, Show,-} Generic)

instance FromJSON UnsignedData where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON UnsignedData where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data EventContent = EventContent
  { _eventContent_avatar_url :: MatrixUri
  , _eventContent_displayname :: Maybe Text
  , _eventContent_membership :: Membership
  , _eventContent_isDirect :: Bool
  , _eventContent_thirdPartyInvite :: Invite
  , _eventContent_unsigned ::UnsignedData
  } deriving ({-Eq, Ord, Show,-} Generic)

instance FromJSON EventContent where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON EventContent where
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
  , _signed_token :: Text
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

data DefiniteEvent meta body = DefiniteEvent
  { _definiteEvent_content :: body
  , _definiteEvent_type :: EventType '(meta, body)
  , _definiteEvent_extraFields :: meta
  } deriving ({-Eq, Ord, Show,-} Generic)

data Event' ctr = forall meta body. Event'
  { _event_event :: DefiniteEvent meta body
  , _event_constraint :: Dict (ctr meta)
  } --deriving (Eq, Ord, Show)

instance FromJSON (Event' ctr) where
  parseJSON = undefined -- TODO
instance ToJSON (Event' ctr) where
  toJSON = undefined -- TODO

--------------------------------------------------------------------------------

class Unconstrained a
instance Unconstrained a

type Event = Event' Unconstrained

--------------------------------------------------------------------------------

data StateEventMeta = StateEventMeta
  { _stateEventMeta_stateKey :: StateKey -- REQUIRED
  , _stateEventMeta_eventId :: EventId -- REQUIRED
  , _stateEventMeta_sender :: Text -- REQUIRED
  , _stateEventMeta_originServerAs :: Int32 -- REQUIRED
  , _stateEventMeta_unsigned :: Maybe UnsignedData -- REQUIRED
  , _stateEventMeta_prevContent :: Maybe EventContent -- REQUIRED
  } deriving ({-Eq, Ord, Show,-} Generic)

makeClassy ''StateEventMeta

instance FromJSON StateEventMeta where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON StateEventMeta where
  toJSON = genericToJSON aesonOptions

type StateEvent = Event' HasStateEventMeta

--------------------------------------------------------------------------------

data RoomEventMeta = RoomEventMeta
  { _roomEventMeta_eventId :: EventId -- REQUIRED
  , _roomEventMeta_sender :: Text -- REQUIRED
  , _roomEventMeta_unsigned :: UnsignedData -- REQUIRED
  } deriving ({-Eq, Ord, Show,-} Generic)

makeClassy ''RoomEventMeta

instance FromJSON RoomEventMeta where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON RoomEventMeta where
  toJSON = genericToJSON aesonOptions

type RoomEvent = Event' HasRoomEventMeta

--------------------------------------------------------------------------------

join <$> traverse (\ty -> liftA2 (<>) (makeLenses ty) (makeFields ty))
  [ ''UnsignedData
  , ''EventContent
  , ''Invite
  , ''DefiniteEvent
  , ''Event'
  ]
