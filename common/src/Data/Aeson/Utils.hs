module Data.Aeson.Utils where

import Control.Applicative
import Data.Aeson
import Data.Text (Text)

data ErrorBadJsonParse = ErrorBadJsonParse
  { _ErrorBadJsonParse_error :: String
  , _ErrorBadJsonParse_raw :: Text
  }
  deriving (Eq, Ord, Show)

(.=?) :: (Alternative f, KeyValue kv, ToJSON v) => Text -> Maybe v -> f kv
(.=?) k = \case
  Just v -> pure $ k .= v
  Nothing -> empty
