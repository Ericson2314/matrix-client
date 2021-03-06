{-# LANGUAGE TemplateHaskell #-}
module Matrix.Identifiers where

import           Control.Applicative
import           Control.Lens.TH
import           Data.Aeson
import           Data.Aeson.Types (toJSONKeyText)
import qualified Data.Aeson.Types as Aeson (Parser)
import           Data.Attoparsec.Text
import           Data.Char
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.DependentXhr (ToRoutePiece (..))
import           Data.Word
import           GHC.Generics
import           Net.IPv4
import           Net.IPv6
import           Text.Read (readMaybe)

data Host
  = Host_Domain (NonEmpty Text)
  | Host_IPv4 IPv4
  | Host_IPv6 IPv6
  deriving (Eq, Ord, Show, Generic)

printHost :: Host -> Text
printHost = \case
  Host_Domain (d :| ds) -> T.intercalate "." $ d : ds
  Host_IPv4 i -> T.pack $ show i
  Host_IPv6 i -> T.pack $ show i

parseHost :: Parser Host
parseHost = ipv4address <|> (char '[' *> ipv6address <* char ']' ) <|> dnsName
  where
    ipv4address = Host_IPv4 <$> Net.IPv4.parser
    ipv6address = Host_IPv6 <$> Net.IPv6.parser
    dnsName = Host_Domain <$> do
      let bit = takeWhile1 $ \c -> isAlphaNum c || isDigit c || c == '-'
      b <- bit
      bs <- some (char '.' *> bit)
      pure $ b :| bs

instance ToRoutePiece Host where
  toRoute = toRoute

--------------------------------------------------------------------------------

data ServerName = ServerName
  { _serverName_host :: Host
  , _serverName_port :: Maybe Word16
  }
  deriving (Eq, Ord, Show, Generic)
makeLenses ''ServerName

printServerName :: ServerName -> Text
printServerName (ServerName h mPort) = printHost h
  <> maybe "" ((":" <>) . T.pack . show) mPort

parseServerName :: Parser ServerName
parseServerName = ServerName <$> parseHost <*> optional (char ':' *> port)
  where
    -- TODO out of bounds
    port :: Parser Word16 = do
      Just t <- readMaybe <$> many1 (satisfy isDigit)
      pure t

runParserJson :: Parser a -> Text -> Aeson.Parser a
runParserJson p t = case parseOnly p t of
  Left e -> fail e
  Right sn -> pure sn

instance ToRoutePiece ServerName where
  toRoute = printServerName

instance ToJSON ServerName where
  toJSON = toJSON . printServerName
instance ToJSONKey ServerName where
  toJSONKey = toJSONKeyText printServerName
instance FromJSON ServerName where
  parseJSON = withText "server name" $ runParserJson parseServerName
instance FromJSONKey ServerName where
  fromJSONKey = FromJSONKeyTextParser $ runParserJson parseServerName

--------------------------------------------------------------------------------

newtype UserName = UserName { getUserName :: Text }
  deriving (Eq, Ord, Show, Generic)

printUserName :: UserName -> Text
printUserName = getUserName

parseUserName :: Parser UserName
parseUserName = fmap UserName $ takeWhile1 $ \c -> isDigit c
  || (ord c >= 0x61 && ord c <= 0x7A)
  || elem c ['-', '.', '=', '_', '/']

instance ToRoutePiece UserName where
  toRoute = printUserName

instance ToJSON UserName where
  toJSON sn = toJSON $ printUserName sn
instance ToJSONKey UserName where
  toJSONKey = toJSONKeyText printUserName
instance FromJSON UserName where
  parseJSON = withText "user ID" $ runParserJson parseUserName
instance FromJSONKey UserName where
  fromJSONKey = FromJSONKeyTextParser $ runParserJson parseUserName

--------------------------------------------------------------------------------

data UserId = UserId
  { _userId_username :: UserName
  , _userId_domain :: ServerName
  }
  deriving (Eq, Ord, Show, Generic)
makeLenses ''UserId

printUserId :: UserId -> Text
printUserId (UserId u d) = "@" <> printUserName u <> ":" <> printServerName d

parseUserId :: Parser UserId
parseUserId = do
  _ <- char '@'
  u <- parseUserName
  _ <- char ':'
  sn <- parseServerName
  pure $ UserId u sn

instance ToRoutePiece UserId where
  toRoute = printUserId

instance ToJSON UserId where
  toJSON sn = toJSON $ printUserId sn
instance ToJSONKey UserId where
  toJSONKey = toJSONKeyText printUserId
instance FromJSON UserId where
  parseJSON = withText "user ID" $ runParserJson parseUserId
instance FromJSONKey UserId where
  fromJSONKey = FromJSONKeyTextParser $ runParserJson parseUserId

--------------------------------------------------------------------------------

newtype RoomName = RoomName { getRoomName :: Text }
  deriving (Eq, Ord, Show, Generic)

printRoomName :: RoomName -> Text
printRoomName = getRoomName

-- | TODO find right grammar for this.
parseRoomName :: Parser RoomName
parseRoomName = fmap RoomName $ takeWhile1 (/= ':')

instance ToRoutePiece RoomName where
  toRoute = printRoomName

instance ToJSON RoomName where
  toJSON sn = toJSON $ printRoomName sn
instance ToJSONKey RoomName where
  toJSONKey = toJSONKeyText printRoomName
instance FromJSON RoomName where
  parseJSON = withText "user ID" $ runParserJson parseRoomName
instance FromJSONKey RoomName where
  fromJSONKey = FromJSONKeyTextParser $ runParserJson parseRoomName

--------------------------------------------------------------------------------

data RoomAlias = RoomAlias
  { _roomAlias_name :: RoomName
  , _roomAlias_domain :: ServerName
  }
  deriving (Eq, Ord, Show, Generic)
makeLenses ''RoomAlias

printRoomAlias :: RoomAlias -> Text
printRoomAlias (RoomAlias u d) = "#" <> printRoomName u <> ":" <> printServerName d

parseRoomAlias :: Parser RoomAlias
parseRoomAlias = do
  _ <- char '#'
  u <- parseRoomName
  _ <- char ':'
  sn <- parseServerName
  pure $ RoomAlias u sn

instance ToRoutePiece RoomAlias where
  toRoute = printRoomAlias

instance ToJSON RoomAlias where
  toJSON sn = toJSON $ printRoomAlias sn
instance ToJSONKey RoomAlias where
  toJSONKey = toJSONKeyText printRoomAlias
instance FromJSON RoomAlias where
  parseJSON = withText "user ID" $ runParserJson parseRoomAlias
instance FromJSONKey RoomAlias where
  fromJSONKey = FromJSONKeyTextParser $ runParserJson parseRoomAlias

--------------------------------------------------------------------------------

data RoomId = RoomId
  { _roomId_opaque :: Text
  , _roomId_domain :: ServerName
  }
  deriving (Eq, Ord, Show, Generic)
makeLenses ''RoomId

printRoomId :: RoomId -> Text
printRoomId (RoomId u d) = "@" <> u <> ":" <> printServerName d

parseRoomId :: Parser RoomId
parseRoomId = do
  _ <- char '!'
  u <- takeWhile1 (/= ':')
  _ <- char ':'
  sn <- parseServerName
  pure $ RoomId u sn

instance ToRoutePiece RoomId where
  toRoute = printRoomId

instance ToJSON RoomId where
  toJSON sn = toJSON $ printRoomId sn
instance ToJSONKey RoomId where
  toJSONKey = toJSONKeyText printRoomId
instance FromJSON RoomId where
  parseJSON = withText "room ID" $ runParserJson parseRoomId
instance FromJSONKey RoomId where
  fromJSONKey = FromJSONKeyTextParser $ runParserJson parseRoomId

--------------------------------------------------------------------------------

data EventId = EventId
  { _eventId_opaque :: Text
  , _eventId_domain :: ServerName
  }
  deriving (Eq, Ord, Show, Generic)
makeLenses ''EventId

printEventId :: EventId -> Text
printEventId (EventId u d) = "@" <> u <> ":" <> printServerName d

parseEventId :: Parser EventId
parseEventId = do
  _ <- char '!'
  u <- takeWhile1 (/= ':')
  _ <- char ':'
  sn <- parseServerName
  pure $ EventId u sn

instance ToRoutePiece EventId where
  toRoute = printEventId

instance ToJSON EventId where
  toJSON sn = toJSON $ printEventId sn
instance ToJSONKey EventId where
  toJSONKey = toJSONKeyText printEventId
instance FromJSON EventId where
  parseJSON = withText "room ID" $ runParserJson parseEventId
instance FromJSONKey EventId where
  fromJSONKey = FromJSONKeyTextParser $ runParserJson parseEventId

--------------------------------------------------------------------------------

newtype DeviceId = DeviceId { unDeviceId :: Text }
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

--------------------------------------------------------------------------------

newtype TxnId = TxnId { unTxnId :: Text }
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (FromJSON, ToJSON)
