{-# LANGUAGE TemplateHaskell #-}
module Frontend.Query where

import           Data.Align
import           Data.DeriveTH
import           Data.Functor.Identity
import           Data.Map (Map)
import           Data.Semigroup
import           Obelisk.Database.Beam.Entity
import           Reflex

import           Frontend.Schema

type EntityMap table = Map (Key table Identity) (table Identity)

data FrontendQuery a = FrontendQuery
  { _frontendQuery_login :: Maybe a
  } deriving (Eq, Ord, Show, Functor)

data FrontendQueryResult = FrontendQueryResult
  { _frontendQueryResult_login :: Option (First (EntityMap Login))
  }

instance FunctorMaybe FrontendQuery where
  fmapMaybe f q = FrontendQuery
    { _frontendQuery_login = fmapMaybe f $ _frontendQuery_login q
    }

instance Align FrontendQuery where
  nil = FrontendQuery
    { _frontendQuery_login = nil
    }
  alignWith f a b = FrontendQuery
    { _frontendQuery_login = alignWith f (_frontendQuery_login a) (_frontendQuery_login b)
    }

instance (Monoid a, Eq a) => Semigroup (FrontendQuery a) where
  (<>) = mappend

instance (Monoid a, Eq a) => Monoid (FrontendQuery a) where
  mempty = nil
  mappend a b = fmapMaybe dropMempty $ salign a b
   where
    dropMempty x = if x == mempty then Nothing else Just x

instance (Group a, Eq a) => Group (FrontendQuery a) where
  negateG = fmap negateG

instance (Additive a, Monoid a, Eq a) => Additive (FrontendQuery a) where

instance Group a => Query (FrontendQuery a) where
  type QueryResult (FrontendQuery a) = FrontendQueryResult
  crop q qr = FrontendQueryResult
    { _frontendQueryResult_login =
        Option (_frontendQuery_login q) >> _frontendQueryResult_login qr
    }

instance Semigroup FrontendQueryResult where
  (<>) = mappend

derive makeMonoid ''FrontendQueryResult
