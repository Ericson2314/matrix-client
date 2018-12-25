{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}
module Matrix.Client.Types
  ( module Matrix.Client.Types
  , module Matrix.Client.Types.Auth.Login
  , module Matrix.Client.Types.Auth.Account
  , module Matrix.Client.Types.Filter
  , module Matrix.Client.Types.Event
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
import           Matrix.Client.Types.Filter
import           Matrix.Client.Types.Event

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
  ClientServerRoute_Filter
    :: FilterRoute httpType route needsAuth request respPerCode
    -> ClientServerRoute
       httpType
       (Prefix route)
       needsAuth
       request
       respPerCode
  ClientServerRoute_Event
    :: EventRoute httpType route needsAuth request respPerCode
    -> ClientServerRoute
       httpType
       (Prefix route)
       needsAuth
       request
       respPerCode
  ClientServerRoute_Join
    :: ClientServerRoute
       'PUT
       ['Left "_matrix", 'Left "client", 'Left "r0", 'Left "rooms", 'Right RoomId, 'Left "join"]
       'True
       JoinRequest
       JoinRespKey

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
  [ ''JoinRespKey
  ]

join <$> traverse (\ty -> liftA2 (<>) (makeLenses ty) (makeFields ty))
  [ ''JoinRequest, ''JoinResponse
  ]
