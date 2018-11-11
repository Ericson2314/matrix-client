{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Database.Beam.Matrix.Orphans where

import Database.Beam.Migrate

import Matrix.Client.Types

deriving newtype instance BeamMigrateSqlBackend be => HasDefaultSqlDataTypeConstraints be DeviceId
deriving newtype instance BeamMigrateSqlBackend be => HasDefaultSqlDataType be DeviceId

deriving newtype instance BeamMigrateSqlBackend be => HasDefaultSqlDataTypeConstraints be UserId
deriving newtype instance BeamMigrateSqlBackend be => HasDefaultSqlDataType be UserId
