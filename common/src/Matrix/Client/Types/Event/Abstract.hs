{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
module Matrix.Client.Types.Event.Abstract where

import           Control.Lens hiding ((.=), (%~))
import           Control.Applicative (liftA2)
import           Control.Monad
import           Data.Aeson
import qualified Data.Aeson as Ae
import           Data.Constraint.Extras.TH
import           Data.GADT.Compare
import           Data.GADT.Compare.TH
import           Data.GADT.Show
import           Data.GADT.Show.TH
import           Data.Constraint
import           Data.Kind
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Some
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Int
import           Data.Word
import           GHC.Generics
import           GHC.TypeLits

import           Data.DependentXhr
import           Matrix.Identifiers
import           Matrix.Client.Types.Common

--------------------------------------------------------------------------------

-- TODO uncurry once GADT instance TH and vessel is more flexible
type EventTypeKind = (Type, Type) -> Type

--------------------------------------------------------------------------------

data DefiniteEventAbstract (et :: EventTypeKind) meta body
  = DefiniteEventAbstract
    { _definiteEvent_content :: body
    , _definiteEvent_type :: et '(meta, body)
    , _definiteEvent_extraFields :: meta
    }

deriving instance
  ( Eq meta
  , Eq body
  , Eq (et '(meta, body))
  ) => Eq (DefiniteEventAbstract et meta body)
deriving instance
  ( Ord meta
  , Ord body
  , Ord (et '(meta, body))
  ) => Ord (DefiniteEventAbstract et meta body)
deriving instance
  ( Show meta
  , Show body
  , Show (et '(meta, body))
  ) => Show (DefiniteEventAbstract et meta body)
deriving instance Generic (DefiniteEventAbstract et meta body)

instance
  ( FromJSON meta
  , FromJSON body
  , FromJSON (et '(meta, body))
  ) => FromJSON (DefiniteEventAbstract et meta body) where
  parseJSON = genericParseJSON aesonOptions
instance
  ( ToJSON meta
  , ToJSON body
  , ToJSON (et '(meta, body))
  ) => ToJSON (DefiniteEventAbstract et meta body) where
  toJSON = genericToJSON aesonOptions

data EventAbstract (et :: EventTypeKind) ctr
  = forall meta body. EventAbstract
    { _event_event :: DefiniteEventAbstract et meta body
    , _event_constraint :: Dict (ctr meta body)
    } --deriving (Eq, Ord, Show, Generic)

instance FromJSON (EventAbstract et ctr) where
  parseJSON = undefined -- TODO
instance ToJSON (EventAbstract et ctr) where
  toJSON = undefined -- TODO

--------------------------------------------------------------------------------

class Unconstrained a b
instance Unconstrained a b

--------------------------------------------------------------------------------

join <$> traverse (\ty -> liftA2 (<>) (makeLenses ty) (makeFields ty))
  [ ''DefiniteEventAbstract
  , ''EventAbstract
  ]
