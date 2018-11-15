module Frontend where

import Control.Lens
import Control.Monad.IO.Class
import Data.Text (Text)
import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.SemanticUI

import Common.Route

import Frontend.Request
import Frontend.Request.Local

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = el "title" $ text "Matrix"
  , _frontend_body = mapRoutedT runLocalFrontendRequestT $ subRoute_ $ \case
      FrontendRoute_Home -> homePage
      FrontendRoute_Login -> loginPage
  }

homePage :: ObeliskWidget t x (R FrontendRoute) m => m ()
homePage = do
  login <- button def $ text "Login"
  setRoute $ FrontendRoute_Login :/ () <$ login
  return ()

loginPage :: (ObeliskWidget t x (R FrontendRoute) m, MonadFrontendRequest t m) => m ()
loginPage = do
  homeServerEl <- input def $ inputElement $ def
    & inputElementConfig_initialValue .~ "https://matrix.org"
    & placeholder .~ Just "Home server"
  userNameEl <- input def $ inputElement $ def
    & placeholder .~ Just "User name"
  passwordEl <- input def $ inputElement $ def
    & placeholder .~ Just "Password"

  login <- button def $ text "Login"
  loginResult <- performFrontendRequest $ flip pushAlways login $ const $
    FrontendRequest_Login
      <$> (sample $ current $ value homeServerEl)
      <*> (sample $ current $ value userNameEl)
      <*> (sample $ current $ value passwordEl)

  setRoute $ FrontendRoute_Home :/ () <$ filterRight loginResult
  prerender blank $ performEvent_ $ liftIO . print <$> filterLeft loginResult

placeholder :: Lens' (InputElementConfig er t s) (Maybe Text)
placeholder = inputElementConfig_elementConfig . elementConfig_initialAttributes . at "placeholder"
