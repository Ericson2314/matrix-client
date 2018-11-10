{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import qualified Reflex.Dom.SemanticUI as SUI

import Common.Route

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = el "title" $ text "Matrix"
  , _frontend_body =
      subRoute_ $ \case
        FrontendRoute_Home -> do
          login <- SUI.button def $ text "Login"
          setRoute $ FrontendRoute_Login :/ () <$ login
          return ()
        FrontendRoute_Login -> do
          blank
  }
