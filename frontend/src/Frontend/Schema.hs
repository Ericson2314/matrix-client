{-# LANGUAGE TemplateHaskell #-}
module Frontend.Schema where

import Control.Lens
import Data.Text
import Database.Beam
import Database.Beam.Keyed
import Database.Beam.Migrate
import Database.Beam.Sqlite

import qualified Matrix.Identifiers as M

import Database.Beam.Matrix.Orphans ()
import Data.DependentXhr

data LoginT f = Login
  { _login_homeServer :: Columnar f Text
  , _login_accessToken :: Columnar f AccessToken
  , _login_deviceId :: Columnar (Nullable f) M.DeviceId
  , _login_isActive :: Columnar f Bool
  } deriving (Generic, Beamable)
type Login = LoginT Identity

-- TODO forall constraint
deriving instance Eq Login
deriving instance Ord Login
deriving instance Show Login

-- Until https://github.com/tathougies/beam/issues/262 is resolved, this is too
-- annoying.
type instance Id LoginT = Text -- M.UserId
instance HasKey LoginT

makeLenses ''LoginT

data Db f = Db
  { _db_login :: f (TableEntity (KeyedT LoginT))
  } deriving (Generic, Database be)

db :: DatabaseSettings Sqlite Db
db = unCheckDatabase checkedDb

checkedDb :: CheckedDatabaseSettings Sqlite Db
checkedDb = withDbModification defaultMigratableDbSettings $ Db
  { _db_login = checkedKeyed "login" (IdKey $ checkedFieldNamed "user_id") $ Login
      { _login_homeServer = checkedFieldNamed "home_server"
      , _login_accessToken = checkedFieldNamed "access_token"
      , _login_deviceId = checkedFieldNamed "device_id"
      , _login_isActive = checkedFieldNamed "is_active"
      }
  }
