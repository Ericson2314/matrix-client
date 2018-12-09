{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Obelisk.Route.Frontend.Logger.Orphans where

import Control.Monad.Logger
import Obelisk.Route.Frontend

instance MonadLogger m => MonadLogger (RoutedT t r m) where
