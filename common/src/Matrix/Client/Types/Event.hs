{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

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
import           Data.GADT.Show
import           Data.Kind
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Singletons
import           Data.Singletons.TH
import           Data.Some
import           Data.Text (Text)
import           Data.Int
import           Data.Word
import           GHC.Generics
import           GHC.TypeLits

import           Data.DependentXhr
import           Matrix.Identifiers
import           Matrix.Client.Types.Common

--------------------------------------------------------------------------------

$(promote [d|
  data EventType
    = EventType_Unknown Text
    deriving (Eq, Ord, Show, Generic)
  |])

class ( Eq (EventBody et)
      , Show (EventBody et)
      , Ord (EventBody et)
      )
  => IsEvent (et :: EventType)
  where
    type EventBody et = e | e -> et

class IsEvent et => IsStateEvent (et :: EventType)
class IsEvent et => IsMessageEvent (et :: EventType)
class IsEvent et => IsRoomEvent (et :: EventType)

instance ToRoutePiece EventType where
  toRoute = \case
    EventType_Unknown txt -> txt

instance ToJSON EventType where
  toJSON = Ae.String . toRoute
instance FromJSON EventType where
  parseJSON = undefined -- TOOD

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
  } deriving (Eq, Ord, Show, Generic)

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
  } deriving (Eq, Ord, Show, Generic)

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

data DefiniteEvent et = DefiniteEvent
  { _event_content :: EventBody et
  , _event_type :: Sing et
  , _event_roomId :: IsRoomEvent et => RoomId
  , _event_stateKey :: IsStateEvent et => StateKey
  , _event_eventId :: IsStateEvent et => EventId
  , _event_sender :: IsStateEvent et => Text
  , _event_originServerAs :: IsStateEvent et => Int32
  , _event_unsigned :: IsStateEvent et => Maybe UnsignedData
  , _event_prevContent :: IsStateEvent et => Maybe EventContent
  } deriving ()

deriving instance IsEvent et => Eq (DefiniteEvent et)
deriving instance IsEvent et => Ord (DefiniteEvent et)
deriving instance IsEvent et => Show (DefiniteEvent et)

instance FromJSON (DefiniteEvent et) where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON (DefiniteEvent et) where
  toJSON = genericToJSON aesonOptions

instance GEq DefiniteEvent where
  geq a b = case _event_type a %~ _event_type b of
    Proved Refl -> Just Refl
    Nothing -> Nothing

instance GCompare DefiniteEvent where
  gcompare a b = case _event_type a %~ _event_type b of
    Proved Refl -> case a `compare` b of
      LT -> GLT
      EQ -> GEq
      GT -> GGT
    Nothing -> case unsing (_event_type a) `compare` unsing (_event_type b) of
      LT -> GLT
      EQ -> error "not possible"
      GT -> GGT

instance GShow DefiniteEvent where

type Event = Some DefiniteEvent
