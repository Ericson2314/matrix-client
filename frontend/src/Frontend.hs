module Frontend where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Vessel
import           Obelisk.Frontend
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Obelisk.Route.Frontend.Logger.Orphans ()
import           Reflex.Dom.SemanticUI

import           Common.Route

import           Frontend.Query
import           Frontend.Request
import           Frontend.Request.Local

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = el "title" $ text "Matrix"
  , _frontend_body = mapRoutedT runLocalFrontendRequestT $ subRoute_ $ \case
      FrontendRoute_Home -> homePage
      FrontendRoute_Login -> loginPage
  }

homePage :: (ObeliskWidget t x (R FrontendRoute) m, MonadQuery t FrontendQuery m) => m ()
homePage = do
  login <- button def $ text "Login"
  dLoginIds <- queryLogins
  el "ul" $
    void $ dyn $ ffor dLoginIds $ mapM_ $ mapM_ $ \loginId -> do
      dml <- queryLogin (pure $ Just loginId)
      void $ dyn $ ffor dml $ mapM_ $ \l ->
        text $ T.pack $ show l
  setRoute $ FrontendRoute_Login :/ () <$ login

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
  loginResult <- performFrontendRequest $ flip tag login $
    FrontendRequest_Login
      <$> (current $ value homeServerEl)
      <*> (current $ value userNameEl)
      <*> (current $ value passwordEl)

  setRoute $ FrontendRoute_Home :/ () <$ filterRight loginResult
  prerender blank $ performEvent_ $ liftIO . print <$> filterLeft loginResult

placeholder :: Lens' (InputElementConfig er t s) (Maybe Text)
placeholder = inputElementConfig_elementConfig . elementConfig_initialAttributes . at "placeholder"
