{-# LANGUAGE TemplateHaskell #-}
module Frontend.Schema where

import Control.Lens
import Data.Text
import Database.Beam
import Database.Beam.Migrate
import Database.Beam.Sqlite
import Obelisk.Database.Beam.Entity

import Database.Beam.Matrix.Orphans ()
import qualified Matrix.Client.Types as M

data Login f = Login
  { _login_homeServer :: Columnar f Text
  , _login_accessToken :: Columnar (Nullable f) Text
  , _login_deviceId :: Columnar (Nullable f) M.DeviceId
  , _login_isActive :: Columnar f Bool
  } deriving (Generic, Beamable)
mkLenses ''Login


type instance Key Login = Id M.UserId

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
