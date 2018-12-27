{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}
module Matrix.Client.Types
  ( module Matrix.Client.Types
  , module Matrix.Client.Types.Auth.Login
  , module Matrix.Client.Types.Auth.Account
  , module Matrix.Client.Types.Filter
  , module Matrix.Client.Types.Event
  , module Matrix.Client.Types.Event.Route
  , module Matrix.Client.Types.Room
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
import           Data.Proxy
import           Data.Some
import           Data.Text (Text)
import           Data.Type.Equality
import           Data.Int
import           Data.Word
import           GHC.Generics
import           GHC.TypeLits

import           Matrix.Identifiers
import           Matrix.Client.Types.Common
import           Matrix.Client.Types.Auth.Login
import           Matrix.Client.Types.Auth.Account
import           Matrix.Client.Types.Filter
import           Matrix.Client.Types.Event
import           Matrix.Client.Types.Event.Route
import           Matrix.Client.Types.Room

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
  ClientServerRoute_Room
    :: RoomRoute httpType route needsAuth request respPerCode
    -> ClientServerRoute
       httpType
       (Prefix route)
       needsAuth
       request
       respPerCode
