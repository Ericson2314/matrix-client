{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Frontend.Query.Sync where

import           Control.Monad
import           Data.Constraint.Extras.TH
import           Data.Functor.Identity
import           Data.GADT.Compare.TH
import qualified Data.Map.Monoidal as MM
import           Data.Semigroup
import           Data.Set
import           Data.Vessel
import           GHC.Generics
import           Obelisk.Database.Beam.Entity
import           Reflex

import           Matrix.Client.Types.Filter
import           Matrix.Client.Types.Event.Route

-- TODO be even remotely fine-grained
data RawMatrixClientV g = RawMatrixClientV
  { _rawMatrixClientV_selector :: Filter'
  , _rawMatrixClientV_selectee :: g SyncResponse
  } deriving (Group, Additive, Generic)

deriving instance Eq (g SyncResponse) => Eq (RawMatrixClientV g)
deriving instance Ord (g SyncResponse) => Ord (RawMatrixClientV g)
deriving instance Show (g SyncResponse) => Show (RawMatrixClientV g)

-- We really want a lattice instead, this is union
instance Applicative g => Semigroup (RawMatrixClientV g) where
  f0 <> f1 = RawMatrixClientV
    { _rawMatrixClientV_selector = Filter
      { _putFilterRequest_eventFields = do
          ev0 <- _putFilterRequest_eventFields $ _rawMatrixClientV_selector f0
          ev1 <- _putFilterRequest_eventFields $ _rawMatrixClientV_selectorf1
          pure $ ev0 <> ev1
      , _putFilterRequest_eventFormat = f0 -- be communative, use another type
      , _putFilterRequest_presence = (<>)
        <$> _putFilterRequest_presence $ _rawMatrixClientV_selector f0
        <*> _putFilterRequest_presence $ _rawMatrixClientV_selector f1
      , _putFilterRequest_accountData = (<>)
        <$> _putFilterRequest_accountData $ _rawMatrixClientV_selectorf0
        <*> _putFilterRequest_accountData $ _rawMatrixClientV_selectorf1
      , _putFilterRequest_roomFilter = (<>)
        <$> _putFilterRequest_roomFilter $ _rawMatrixClientV_selector f0
        <*> _putFilterRequest_roomFilter $ _rawMatrixClientV_selector f1
      }
    , _rawMatrixClientV_selectee = liftA2 (<>)
      <$> _rawMatrixClientV_selectee f0
      <$> _rawMatrixClientV_selectee f1
    }

-- We really want a lattice instead, this is union
instance Applicative g => Monoid (RawMatrixClientV g) where
  mempty = Filter
    { _putFilterRequest_eventFields = Just []
    , _putFilterRequest_eventFormat = Nothing
    , _putFilterRequest_presence = Nothing
    , _putFilterRequest_accountData = Nothing
    , _putFilterRequest_roomFilter = Nothing
    }
