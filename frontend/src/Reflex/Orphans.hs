{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeInType #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Reflex.Orphans where

import           Control.Monad.Morph
import           Reflex

-- TODO upstream
instance MFunctor (EventWriterT t w) where
  hoist nt (EventWriterT m) = EventWriterT $ hoist nt m

-- TODO upstream
instance MFunctor (QueryT t q) where
  hoist nt (QueryT m) = QueryT $ hoist (hoist (hoist nt)) m
