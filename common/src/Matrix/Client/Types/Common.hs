{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Matrix.Client.Types.Common where

import           Data.Aeson
import qualified Data.Aeson as Ae
import           Data.Text (Text)
import           Data.Void
import           GHC.Generics
import           Text.Megaparsec (Parsec, parseMaybe)
import           Text.URI

--------------------------------------------------------------------------------

aesonOptions :: Ae.Options
aesonOptions = defaultOptions { Ae.fieldLabelModifier = unCamelPrefixedField }

-- | Strips a field name of its type name and separating underscore (optionally
-- preceeded by an initial underscore as well) and converts CamelCase to
-- lowercase_with_underscores.
unCamelPrefixedField :: String -> String
unCamelPrefixedField =
  camelTo2 '_' . tail . dropWhile (/= '_') . dropWhile (== '_')

--------------------------------------------------------------------------------

-- for JSON instances.
newtype MatrixUri = MatrixUri { unMatrixUri :: URI }
  deriving (Eq, Ord, Show, Generic)

-- TODO megaparsec -> aeson parser
instance FromJSON MatrixUri where
  parseJSON = withText "URL" $ \t -> do
    Just a <- pure $ parseMaybe (Text.URI.parser :: Parsec Void Text URI) t
    pure $ MatrixUri a

instance ToJSON MatrixUri where
  toJSON = String . render . unMatrixUri
