{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Matrix.Client.Types.Common where

import           Control.Lens hiding ((.=))
import           Control.Applicative (liftA2)
import           Control.Monad
import           Data.Aeson
import qualified Data.Aeson as Ae
import           Data.Aeson.Utils
import           Data.Constraint.Extras.TH
import           Data.DependentXhr
import           Data.Kind
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Some
import           Data.Text (Text)
import           Data.Traversable
import           Data.Int
import           Data.Word
import           Data.Void
import           GHC.Generics
import           Text.Megaparsec (Parsec, parseMaybe)
import           Text.URI

import           Matrix.Identifiers

--------------------------------------------------------------------------------

aesonOptions :: Ae.Options
aesonOptions = defaultOptions { Ae.fieldLabelModifier = unCamelPrefixedField }

-- | Strips a field name of its type name and separating underscore (optionally
-- preceeded by an initial underscore as well) and converts CamelCase to
-- lowercase_with_underscores.
unCamelPrefixedField :: String -> String
unCamelPrefixedField =
  camelTo2 '_' . tail . dropWhile (/= '_') . dropWhile (== '_')
