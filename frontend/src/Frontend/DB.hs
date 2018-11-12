module Frontend.DB where

import Database.Beam.Migrate.Simple
import Database.Beam.Sqlite
import Database.Beam.Sqlite.Migrate
import Database.SQLite.Simple as Sqlite

import Frontend.Schema

initDb :: IO Sqlite.Connection
initDb = do
  conn <- open =<< getDbPath
  runBeamSqlite conn $ autoMigrate migrationBackend checkedDb
  return conn

-- TODO: Get correct path for each platform.
getDbPath :: IO FilePath
getDbPath = return "db.sqlite3"
