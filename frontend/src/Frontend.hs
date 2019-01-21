module Frontend where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Attoparsec.Text
import           Data.Functor.Compose
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.List.NonEmpty (NonEmpty (..))
import           Obelisk.Frontend
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Obelisk.Route.Frontend.Logger.Orphans ()
import           Reflex.Dom.Form.FieldWriter (tellFieldErr, withFormFieldsErr)
import qualified Reflex.Dom.Form.Validators as Validator
import           Reflex.Dom.Form.Widgets (formItem, validatedInput)
import           Reflex.Dom.SemanticUI
import qualified Reflex.Dom.TextField as Txt

import           Matrix.Identifiers as M

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

loginPage
  :: ( ObeliskWidget t x (R FrontendRoute) m, MonadFrontendRequest t m
     , DomBuilderSpace m ~ GhcjsDomSpace -- TODO not the best
     )
  => m ()
loginPage = do
  let
    defTxt txt = def & Txt.addLabel (labeled txt) & Txt.setPlaceholder txt
    labeled = el "label" . text
    defaultHomeServer = ServerName
      { _serverName_host = Host_Domain $ "matrix" :| ["org"]
      , _serverName_port = Nothing
      }
  homeServer <- formItem
    $ validatedInput Validator.validateText
    $ defTxt "Host"
    & Txt.setInitial "matrix.org"
    & Txt.setPlaceholder "matrix.org"
  userName <- formItem
    $ validatedInput Validator.validateText
    $ defTxt "User name"
  passwordEl <- input def $ inputElement $ def
    & placeholder .~ Just "Password"
  login <- button def $ text "Login"
  loginResult <- performFrontendRequest $ fmapMaybe id $ flip tag login $ getCompose $
    FrontendRequest_Login
      <$> (Compose $ fmap (view _Right) $ current $ homeServer)
      <*> (Compose $ fmap (view _Right) $ current $ userName)
      <*> (Compose $ fmap Just $ current $ value passwordEl)

  setRoute $ FrontendRoute_Home :/ () <$ filterRight loginResult
  prerender blank $ performEvent_ $ liftIO . print <$> filterLeft loginResult

placeholder :: Lens' (InputElementConfig er t s) (Maybe Text)
placeholder = inputElementConfig_elementConfig . elementConfig_initialAttributes . at "placeholder"
