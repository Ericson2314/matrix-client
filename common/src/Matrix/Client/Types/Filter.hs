{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}
module Matrix.Client.Types.Filter where

import           Control.Lens hiding ((.=))
import           Control.Applicative (liftA2)
import           Control.Monad
import           Data.Aeson
import qualified Data.Aeson as Ae
import           Data.Constraint.Extras.TH
import           Data.Text (Text)
import           Data.Word
import           GHC.Generics

import           Data.DependentXhr
import           Matrix.Identifiers
import           Matrix.Client.Types.Common
import           Matrix.Client.Types.Event

--------------------------------------------------------------------------------

data Filter = Filter
  { _putFilterRequest_eventFields :: [Text] -- TODO way more structure
  , _putFilterRequest_eventFormat :: EventFormat
  , _putFilterRequest_presence :: EventFilter
  , _putFilterRequest_accountData :: EventFilter
  , _putFilterRequest_roomFilter :: RoomFilter
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON Filter where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON Filter where
  toJSON = genericToJSON aesonOptions

data EventFormat
  = EventFormat_Client
  | EventFormat_Federation
  deriving (Eq, Ord, Show, Generic, Bounded, Enum)

-- TODO fix enum instances
instance FromJSON EventFormat where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON EventFormat where
  toJSON = genericToJSON aesonOptions

data EventFilter = EventFilter
  { _eventFilter_limit :: Word32
  , _eventFilter_notSenders :: [UserId]
  , _eventFilter_notTypes :: [SomeEventType]
  , _eventFilter_senders :: Maybe [UserId]
  , _eventFilter_types :: Maybe [SomeEventType]
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON EventFilter where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON EventFilter where
  toJSON = genericToJSON aesonOptions

data RoomFilter = RoomFilter
  { _roomFilter_notRooms :: [RoomId]
  , _roomFilter_rooms :: Maybe [RoomId]
  , _roomFilter_ephemeral :: RoomEventFilter
  , _roomFilter_includeLeave :: Bool
  , _roomFilter_state :: RoomEventFilter
  , _roomFilter_timeline :: RoomEventFilter
  , _roomFilter_accountData :: RoomEventFilter
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON RoomFilter where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON RoomFilter where
  toJSON = genericToJSON aesonOptions

-- TODO deduplicate with `EventFilter`
data RoomEventFilter = RoomEventFilter
  { _roomEventFilter_limit :: Word32
  , _roomEventFilter_notSenders :: [UserId]
  , _roomEventFilter_notTypes :: [SomeEventType]
  , _roomEventFilter_senders :: Maybe [UserId]
  , _roomEventFilter_types :: Maybe [SomeEventType]
  , _roomEventFilter_notRooms :: [RoomId]
  , _roomEventFilter_rooms :: Maybe [RoomId]
  , _roomEventFilter_containsUrl :: Maybe Bool
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON RoomEventFilter where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON RoomEventFilter where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

newtype FilterId = FilterId { unFilterId :: Text }
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

--------------------------------------------------------------------------------

data FilterRoute :: Route where
  FilterRoute_PutFilter
    :: FilterRoute
       'POST
       '[ 'Left "user", 'Right UserId, 'Left "filter" ]
       'True
       PutFilterRequest
       PutFilterRespKey
  FilterRoute_GetFilter
    :: FilterRoute
       'GET
       '[ 'Left "user", 'Right UserId, 'Left "filter", 'Right FilterId ]
       'True
       GetFilterRequest
       GetFilterRespKey

--------------------------------------------------------------------------------

data PutFilterRespKey :: RespRelation where
  PutFilterRespKey_200 :: PutFilterRespKey 200 PutFilterResponse

type PutFilterRequest = Filter

data PutFilterResponse = PutFilterResponse
  { _filter_filterId :: FilterId
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON PutFilterResponse where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON PutFilterResponse where
  toJSON = genericToJSON aesonOptions

--------------------------------------------------------------------------------

data GetFilterRespKey :: RespRelation where
  GetFilterRespKey_200 :: GetFilterRespKey 200 GetFilterResponse
  GetFilterRespKey_404 :: GetFilterRespKey 404 Ae.Value

data GetFilterRequest = GetFilterRequest
  deriving (Eq, Ord, Show, Generic)

instance FromJSON GetFilterRequest where
  parseJSON = genericParseJSON aesonOptions
instance ToJSON GetFilterRequest where
  toJSON = genericToJSON aesonOptions

type GetFilterResponse = Filter

--------------------------------------------------------------------------------

join <$> traverse (deriveArgDict)
  [ ''PutFilterRespKey
  ]

join <$> traverse (\ty -> liftA2 (<>) (makeLenses ty) (makeFields ty))
  [ ''Filter
  , ''PutFilterResponse
  , ''GetFilterRequest
  ]
