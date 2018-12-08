{-# LANGUAGE TemplateHaskell #-}
module Matrix.Identifiers where

import           Control.Applicative
import           Control.Lens.TH
import           Data.Aeson
import           Data.Attoparsec.Text
import           Data.Char
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Word
import           Net.IPv4
import           Net.IPv6
import           Text.Read (readMaybe)

data Host
  = Host_Domain (NonEmpty Text)
  | Host_IPv4 IPv4
  | Host_IPv6 IPv6
  deriving (Eq, Ord, Show)

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

data ServerName = ServerName
  { _serverName_host :: Host
  , _serverName_port :: Maybe Word16
  }
  deriving (Eq, Ord, Show)
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

instance ToJSON ServerName where
  toJSON sn = toJSON $ printServerName sn
instance FromJSON ServerName where
  parseJSON = withText "server name" $ \t -> case parseOnly parseServerName t of
    Left e -> fail e
    Right sn -> pure sn

data UserId = UserId
  { _userId_username :: Text -- good enough for now
  , _userId_domain :: ServerName
  }
  deriving (Eq, Ord, Show)
makeLenses ''UserId


printUserId :: UserId -> Text
printUserId (UserId u d) = u <> printServerName d

parseUserId :: Parser UserId
parseUserId = do
  _ <- char '@'
  u <- parseUser
  _ <- char ':'
  sn <- parseServerName
  pure $ UserId u sn
  where
    parseUser = takeWhile1 $ \c -> isDigit c
      || (ord c >= 0x61 && ord c <= 0x7A)
      || elem c ['-', '.', '=', '_', '/']

instance ToJSON UserId where
  toJSON sn = toJSON $ printUserId sn
instance FromJSON UserId where
  parseJSON = withText "server name" $ \t -> case parseOnly parseUserId t of
    Left e -> fail e
    Right sn -> pure sn