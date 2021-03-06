{-# LANGUAGE TemplateHaskell #-}
module Frontend.Query where

import           Control.Monad
import           Data.Constraint.Extras.TH
import           Data.Functor.Identity
import           Data.GADT.Compare.TH
import qualified Data.Map.Monoidal as MM
import           Data.Semigroup
import           Data.Set
import           Data.Vessel
import           Database.Beam.Keyed
import           Reflex

import           Frontend.Schema

type KeyedMapV table = MapV (Key table) (First (Maybe (table Identity)))

data V f where
  V_Login :: V (KeyedMapV Login)
  V_Logins :: V (SingleV (Set (Key Login)))

deriving instance Eq (V f)
deriving instance Ord (V f)
deriving instance Show (V f)

deriveGEq ''V
deriveGCompare ''V
deriveArgDict ''V

type FrontendV = Vessel V

type FrontendQuery = FrontendV (Const SelectedCount)
type FrontendQuery' = FrontendV Proxy
type FrontendQueryResult = FrontendV Identity

queryLogin
  :: (Reflex t, Monad m, MonadQuery t (Vessel V (Const SelectedCount)) m)
  => Dynamic t (Maybe (Key Login))
  -> m (Dynamic t (Maybe (Login Identity)))
queryLogin dmk = getResult <$> queryDyn query
  where
    query = ffor dmk $ \case
      Just k -> singletonV V_Login $ MapV $ MM.singleton k $ Const 1
      Nothing -> mempty
    getResult dr = ffor2 dr dmk $ \r -> \case
      Just k -> lookupV V_Login r >>= MM.lookup k . unMapV >>= getFirst . runIdentity
      Nothing -> Nothing

queryLogins
  :: (Reflex t, Monad m, MonadQuery t (Vessel V (Const SelectedCount)) m)
  => m (Dynamic t (Maybe (Set (Key Login))))
queryLogins = fmap getResult <$> queryDyn query
  where
    query = pure $ singletonV V_Logins $ SingleV $ Const 1
    getResult = lookupV V_Logins >=> (getFirst . runIdentity . unSingleV)
