{-# LANGUAGE TemplateHaskell #-}
module Frontend.Query where

import           Data.Constraint.Extras.TH
import           Data.Functor.Identity
import           Data.GADT.Compare.TH
import           Data.Semigroup
import           Data.Set
import           Data.Vessel
import           Obelisk.Database.Beam.Entity
import           Reflex

import           Frontend.Schema

type EntityMapV table = MapV (Key table Identity) (First (Maybe (table Identity)))

data V f where
  V_Login :: V (EntityMapV Login)
  V_Logins :: V (SingleV (Set (Key Login Identity)))

deriveGEq ''V
deriveGCompare ''V
deriveArgDictV ''V

type FrontendV = Vessel V

type FrontendQuery = FrontendV (Const SelectedCount)
type FrontendQueryResult = FrontendV Identity
