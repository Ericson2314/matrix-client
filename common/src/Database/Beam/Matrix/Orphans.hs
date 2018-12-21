{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Beam.Matrix.Orphans where

import Data.Text
import Database.Beam
import Database.Beam.Backend
import Database.Beam.Migrate

import Matrix.Client.Types
--import Matrix.Identifiers

import Data.DependentXhr

deriving newtype instance (HasSqlValueSyntax be Text) => HasSqlValueSyntax be DeviceId
deriving newtype instance (BeamSqlBackend be, HasSqlEqualityCheck be Text) => HasSqlEqualityCheck be DeviceId
deriving newtype instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be DeviceId
deriving newtype instance BeamMigrateSqlBackend be => HasDefaultSqlDataTypeConstraints be DeviceId
deriving newtype instance BeamMigrateSqlBackend be => HasDefaultSqlDataType be DeviceId

deriving newtype instance (HasSqlValueSyntax be Text) => HasSqlValueSyntax be AccessToken
deriving newtype instance (BeamSqlBackend be, HasSqlEqualityCheck be Text) => HasSqlEqualityCheck be AccessToken
deriving newtype instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be AccessToken
deriving newtype instance BeamMigrateSqlBackend be => HasDefaultSqlDataTypeConstraints be AccessToken
deriving newtype instance BeamMigrateSqlBackend be => HasDefaultSqlDataType be AccessToken

-- Until https://github.com/tathougies/beam/issues/262 is resolved, this is too
-- annoying.

--deriving newtype instance (HasSqlValueSyntax be Text) => HasSqlValueSyntax be UserId
--deriving newtype instance (BeamSqlBackend be, HasSqlEqualityCheck be Text) => HasSqlEqualityCheck be UserId
--deriving newtype instance (BeamBackend be, FromBackendRow be Text) => FromBackendRow be UserId
--deriving newtype instance BeamMigrateSqlBackend be => HasDefaultSqlDataTypeConstraints be UserId
--deriving newtype instance BeamMigrateSqlBackend be => HasDefaultSqlDataType be UserId
