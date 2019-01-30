{-# LANGUAGE RecursiveDo #-}
module Frontend where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Attoparsec.Text (parseOnly)
import           Data.Either (fromRight)
import           Data.Foldable
import           Data.Maybe (isNothing)
import qualified Data.Map as M
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Obelisk.Database.Beam.Entity
import           Obelisk.Frontend
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Obelisk.Route.Frontend.Logger.Orphans ()
import           Reflex.Dom.SemanticUI

import           Matrix.Client.Types as M hiding (Event)
import           Matrix.Identifiers as M

import           Common.Route

import           Frontend.Query
import           Frontend.Request
import           Frontend.Backend

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = el "title" $ text "Matrix"
  , _frontend_body = mapRoutedT runFrontendBackendT $ subRoute_ $ \case

      FrontendRoute_Home -> homePage
      FrontendRoute_Login -> loginPage
      FrontendRoute_ListPublicRooms -> publicRoomListing
  }

homePage :: (ObeliskWidget t x (R FrontendRoute) m, MonadQuery t FrontendQuery m) => m ()
homePage = do
  login <- button def $ text "Login"
  setRoute $ FrontendRoute_Login :/ () <$ login

  listRooms <- button def $ text "List public rooms"
  setRoute $ FrontendRoute_ListPublicRooms :/ () <$ listRooms

  dLoginIds <- queryLogins
  currentLoginId <- el "aside" $ do
    (loginEl, ()) <- selectElement def $ do
      elAttr "option" (M.singleton "value" "") $ text "Select User"
      dyn_ $ ffor dLoginIds $ mapM_ $ mapM_ $ \(Id userIdText) -> do
        -- TODO make trivial once the key `Id UserId`
        let userId = fromRight (error "invalid user id!") $ parseOnly parseUserId userIdText
        elAttr "option" (M.singleton "value" userIdText) $ text $ printUserId userId
    pure $ ffor (_selectElement_value loginEl) $ \case
      "" -> Nothing
      lid -> Just $ Id lid
  dml <- queryLogin currentLoginId
  void $ dyn $ ffor dml $ mapM_ $ \l ->
    text $ T.pack $ show l
  do
    doSync <- el "label" $ do
      doSync <- input def $ inputElement $ def
        & initialAttributes .~ M.fromList [ ("type", "radio")
                                          , ("name", "sync")
                                          ]
      text "do sync"
      pure doSync
    el "label" $ do
      _ <- input def $ inputElement $ def
        & initialAttributes .~ M.fromList [ ("type", "radio")
                                          , ("name", "sync")
                                          , ("checked", "")
                                          ]
      text "don't sync"
    sync <- querySync $ ffor3 currentLoginId dml (_inputElement_checked doSync) $ \ mUid mL doSync' -> do
      Id u <- mUid
      l <- mL
      guard $ doSync'
      -- TODO make trivial once the key `Id UserId`
      let userId = fromRight (error "invalid user id!") $ parseOnly parseUserId u
      pure (userId, l)
    dyn_ $ ffor sync $ mapM_ $ \s ->
      text $ T.pack $ show $ _syncResponse_nextBatch s
  pure ()

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

publicRoomListing :: (ObeliskWidget t x (R FrontendRoute) m, MonadFrontendRequest t m) => m ()
publicRoomListing = do
  let defaultHomeServer = ServerName
        { _serverName_host = Host_Domain $ "matrix" :| ["org"]
        , _serverName_port = Nothing
        }
  pb <- getPostBuild
  rec
    let dmSince = leftmost
          [ Nothing <$ pb
          , Just <$> prevSince
          , Just <$> nextSince
          ]
    resp <- performFrontendRequest $ FrontendRequest_ListPublicRooms defaultHomeServer 10 <$> dmSince
    let
      f name sinceLens = do
        let emSince = view (_Right . sinceLens) <$> resp
        dmSince' <- holdDyn Nothing $ emSince
        pushed <- flip button (text name) $ def
          { _buttonConfig_disabled = Dyn $ isNothing <$> dmSince'
          }
        pure $ fmapMaybe id $ tag (current dmSince') pushed
    prevSince <- f "Previous" publicRoomsResponse_prevBatch
    nextSince <- f "Next" publicRoomsResponse_nextBatch
  widgetHold_ (text "Getting responses...") $ ffor resp $ \case
    Left error' -> text $ T.pack $ show $ error'
    Right rooms -> do
      let info =
            [ ("aliases", text . T.pack . show . _publicRoomInfo_aliases)
            , ("cannonical alias", text . T.pack . show . _publicRoomInfo_cannonicalAlias)
            , ("name", text . T.pack . show . _publicRoomInfo_name)
            , ("# joined members", text . T.pack . show . _publicRoomInfo_numJoinedMembers)
            , ("room Id", text . T.pack . show . _publicRoomInfo_roomId)
            , ("topic", text . T.pack . show . _publicRoomInfo_topic)
            , ("world readable", text . T.pack . show . _publicRoomInfo_worldReadable)
            , ("guest can join", text . T.pack . show . _publicRoomInfo_guestCanJoin)
            , ("avatar URL", text . T.pack . show . _publicRoomInfo_avatarUrl)
            ]
      el "table" $ do
        el "tr" $ mapM_ (el "th") $ text <$> fst <$> info
        for_ (_publicRoomsResponse_chunk rooms) $ \room -> do
          el "tr" $ do
            mapM_ (el "td") $ ($room) <$> snd <$> info
  pure ()

placeholder :: Lens' (InputElementConfig er t s) (Maybe Text)
placeholder = inputElementConfig_elementConfig . elementConfig_initialAttributes . at "placeholder"
