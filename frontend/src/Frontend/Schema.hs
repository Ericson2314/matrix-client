{-# LANGUAGE TemplateHaskell #-}
module Frontend.Schema where

import Control.Lens
import Data.Text
import Database.Beam
import Database.Beam.Migrate
import Database.Beam.Sqlite
import Obelisk.Database.Beam.Entity

import qualified Matrix.Client.Types as M
import qualified Matrix.Client.Types.Auth.Login as M
import qualified Matrix.Identifiers as M

import Database.Beam.Matrix.Orphans ()
import Data.DependentXhr

data Login f = Login
  { _login_homeServer :: Columnar f Text
  , _login_accessToken :: Columnar (Nullable f) AccessToken
  , _login_deviceId :: Columnar (Nullable f) M.DeviceId
  , _login_isActive :: Columnar f Bool
  } deriving (Generic, Beamable)

makeLenses ''Login

-- Until https://github.com/tathougies/beam/issues/262 is resolved, this is too
-- annoying.
type instance Key Login = Id Text -- M.UserId

data Db f = Db
  { dbLogin :: f (TableEntity (Entity Login))
  } deriving (Generic, Database be)

db :: DatabaseSettings Sqlite Db
db = unCheckDatabase checkedDb

checkedDb :: CheckedDatabaseSettings Sqlite Db
checkedDb = withDbModification defaultMigratableDbSettings $ Db
  { dbLogin = checkedEntity "login" (Id $ checkedFieldNamed "user_id") $ Login
      { _login_homeServer = checkedFieldNamed "home_server"
      , _login_accessToken = checkedFieldNamed "access_token"
      , _login_deviceId = checkedFieldNamed "device_id"
      , _login_isActive = checkedFieldNamed "is_active"
      }
  }
