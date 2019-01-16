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

import           Data.DependentXhr

import           Matrix.Client.Types.Auth.Login
import           Matrix.Client.Types.Auth.Account
import           Matrix.Client.Types.Filter
import           Matrix.Client.Types.Event
import           Matrix.Client.Types.Event.Route
import           Matrix.Client.Types.Room
import           Matrix.Client.Types.UserData

--------------------------------------------------------------------------------

type Prefix r = 'Left "_matrix" ': 'Left "client" ': 'Left "r0" ': r

-- | The Matrix interface for the client to talk to the surver.
data ClientServerRoute :: Route where
  ClientServerRoute_Login
    :: LoginRoute httpType route queryParams needsAuth request respPerCode
    -> ClientServerRoute
       httpType
       (Prefix route)
       queryParams
       needsAuth
       request
       respPerCode
  ClientServerRoute_Account
    :: AccountRoute httpType route queryParams needsAuth request respPerCode
    -> ClientServerRoute
       httpType
       (Prefix route)
       queryParams
       needsAuth
       request
       respPerCode
  ClientServerRoute_Filter
    :: FilterRoute httpType route queryParams needsAuth request respPerCode
    -> ClientServerRoute
       httpType
       (Prefix route)
       queryParams
       needsAuth
       request
       respPerCode
  ClientServerRoute_Event
    :: EventRoute httpType route queryParams needsAuth request respPerCode
    -> ClientServerRoute
       httpType
       (Prefix route)
       queryParams
       needsAuth
       request
       respPerCode
  ClientServerRoute_Room
    :: RoomRoute httpType route queryParams needsAuth request respPerCode
    -> ClientServerRoute
       httpType
       (Prefix route)
       queryParams
       needsAuth
       request
       respPerCode
  ClientServerRoute_UserData
    :: UserDataRoute httpType route queryParams needsAuth request respPerCode
    -> ClientServerRoute
       httpType
       (Prefix route)
       queryParams
       needsAuth
       request
       respPerCode
