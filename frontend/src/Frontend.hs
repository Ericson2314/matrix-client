module Frontend where

import Control.Lens
import Data.Aeson
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text)
import Data.Text.Encoding
import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.SemanticUI

import Matrix.Client.Types
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

loginPage :: (ObeliskWidget t x r m, MonadFrontendRequest t m) => m ()
loginPage = do
  homeServerEl <- input def $ inputElement $ def
    & inputElementConfig_initialValue .~ "https://matrix.org"
    & placeholder .~ Just "Home server"
  userNameEl <- input def $ inputElement $ def
    & placeholder .~ Just "User name"
  passwordEl <- input def $ inputElement $ def
    & placeholder .~ Just "Password"

  login <- button def $ text "Login"
  let request = flip pushAlways login $ const $ do
        hs <- sample $ current $ value homeServerEl
        u <- sample $ current $ value userNameEl
        pw <- sample $ current $ value passwordEl

        let loginReq = LoginRequest
              (UserIdentifier_User u)
              (Login_Password pw)
              Nothing
              Nothing
        let url = hs <> "/_matrix/client/r0/login"
        let body = decodeUtf8 $ toStrict $ Data.Aeson.encode loginReq

        return $ XhrRequest "POST" url $ def
          & xhrRequestConfig_sendData .~ body

  response <- prerender (return never) $
    fmap decodeXhrResponse <$> performRequestAsync request

  performFrontendRequest_ $ FrontendRequest_Login <$> fmapMaybe id response

  el "p" $ do
    v <- holdDyn (Nothing :: Maybe LoginResponse) $ response
    display v

placeholder :: Lens' (InputElementConfig er t s) (Maybe Text)
placeholder = inputElementConfig_elementConfig . elementConfig_initialAttributes . at "placeholder"
