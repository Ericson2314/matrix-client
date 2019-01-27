module Frontend.Backend.Common where

import           Control.Concurrent
import           Control.Lens hiding (has)
import           Control.Monad.Reader
import           Control.Monad.Logger
import qualified Database.SQLite.Simple as Sqlite

import           Frontend.Query

class GivesSQLiteConnection a where
  obtainSqliteConnection :: a -> IO (Maybe Sqlite.Connection)

instance GivesSQLiteConnection Sqlite.Connection where
  obtainSqliteConnection = pure . Just

instance GivesSQLiteConnection (Maybe (MVar Sqlite.Connection)) where
  obtainSqliteConnection = mapM (liftIO . readMVar)

withConnection
  :: (MonadIO m, MonadReader r m, GivesSQLiteConnection r)
  => (Sqlite.Connection -> m a)
  -> m (Maybe a)
withConnection k = do
  traverse k =<< (liftIO . obtainSqliteConnection) =<< ask

withConnectionTransaction
  :: (MonadIO m, MonadReader r m, GivesSQLiteConnection r)
  => (Sqlite.Connection -> IO a)
  -> m (Maybe a)
withConnectionTransaction k = withConnection $ \conn ->
  liftIO $ Sqlite.withTransaction conn $ k conn

class HasQueryCallback a where
  updateQueryResult :: Lens' a (FrontendV Identity -> IO ())

instance HasQueryCallback (FrontendV Identity -> IO ())  where
  updateQueryResult = id

patchQueryResult
  :: (MonadIO m, MonadReader r m, HasQueryCallback r)
  => FrontendV Identity -> m ()
patchQueryResult patch = do
  func <- asks $ view updateQueryResult
  liftIO $ func patch

type Logger = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

class HasLogger a where
  logger :: Lens' a Logger

instance HasLogger Logger where
  logger = id
