{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}
module Matrix.Client.Types.Event where

import           Control.Lens hiding ((.=))
import           Control.Applicative (liftA2)
import           Control.Monad
import           Data.Aeson
import qualified Data.Aeson as Ae
import           Data.Constraint.Extras.TH
import           Data.Kind
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import           Data.Int
import           Data.Word
import           GHC.Generics

import           Data.DependentXhr
import           Matrix.Identifiers
import           Matrix.Client.Types.Common

--------------------------------------------------------------------------------

-- | TODO I think this is just opaque string but double check
newtype StateKey = StateKey { getStateKey :: Text }
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

--------------------------------------------------------------------------------

-- TODO a lot hinges on this, probably use a GADT with unknown raw JSON
-- fallback.
newtype EventType = EventType { getEventType :: Text }
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON)
