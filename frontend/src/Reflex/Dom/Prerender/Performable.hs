-- TODO: Find some way to upstream this?
{-# LANGUAGE RankNTypes #-}
module Reflex.Dom.Prerender.Performable where

import Data.Constraint
import Language.Javascript.JSaddle.Types
import Reflex.Dom.Core

-- | Just the stuff in 'PrerenderClientConstraint' that you get on 'Performable m'.
type JSConstraints js m = (HasJS js m, MonadJSM m, HasJSContext m)

prerenderClientDictPerformable
  :: forall js m. Prerender js m
  => Maybe (Dict (JSConstraints js (Performable m)))
prerenderClientDictPerformable = ffor prerenderClientDict $
  \(Dict :: Dict (PrerenderClientConstraint js m)) -> Dict

-- | Like 'prerender' but for inside a 'Performable m'.
prerenderPerformable
  :: forall js m a. Prerender js m
  => Performable m a
  -> (JSConstraints js (Performable m) => Performable m a)
  -> Performable m a
prerenderPerformable server client = case prerenderClientDictPerformable @js @m of
  Nothing -> server
  Just Dict -> client
