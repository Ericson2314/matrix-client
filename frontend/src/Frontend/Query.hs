{-# LANGUAGE TemplateHaskell #-}
module Frontend.Query where

import           Data.Align
import           Data.Constraint.Extras.TH
import           Data.DeriveTH
import           Data.Functor.Identity
import           Data.GADT.Compare.TH
import           Data.Map (Map)
import           Data.Semigroup
import           Data.Vessel
import           Obelisk.Database.Beam.Entity
import           Reflex

import           Frontend.Schema

type EntityMapV table = MapV (Key table Identity) (First (table Identity))

data FrontendQuery f where
  FrontendQuery_Login :: FrontendQuery (EntityMapV Login)

deriveGEq ''FrontendQuery
deriveGCompare ''FrontendQuery
deriveArgDictV ''FrontendQuery

type FrontendV = Vessel FrontendQuery
