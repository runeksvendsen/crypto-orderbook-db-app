{-# LANGUAGE OverloadedStrings #-}
module Options
( Options(..)
, withArgs
, connString'
)
where

import           Prelude
import qualified Data.ByteString                as BS
import qualified Options.Applicative            as Opt
import qualified Options.Applicative.Types      as Opt
import           Options.Applicative
import qualified CryptoVenues.Venues            as Venues
import qualified Data.Text as T


withArgs :: (Options -> IO a) -> IO a
withArgs f = do
    args <- Opt.execParser opts
    f args

data Options = Options
    { dbConnString      :: BS.ByteString
    , dbMaxRetries      :: Word
    , fetchMaxRetries   :: Word
    , debugMaxBooks     :: Maybe Int
    , disabledVenues    :: [Venues.AnyVenue]
    } deriving (Show)

options :: Opt.Parser Options
options = Options
    <$> connString'
    <*> dbConnRetry'
    <*> fetchMaxRetries'
    <*> optional debugMaxBooks'
    <*> many disableVenue

opts :: Opt.ParserInfo Options
opts = info options $
     fullDesc
  <> progDesc "Fetch orderbooks and write to database"
  <> header "Fetch & write orderbooks"

connString' :: Opt.Parser BS.ByteString
connString' = strOption $
     long "db-conn"
  <> short 'c'
  <> metavar "CONNSTRING"
  <> help "Database connection string (libpq format)"

dbConnRetry' :: Opt.Parser Word
dbConnRetry' = option auto $
     long "db-max-retries"
  <> metavar "CONN_RETRIES"
  <> help "Maximum number of times to retry connecting to database"

fetchMaxRetries' :: Opt.Parser Word
fetchMaxRetries' = option auto $
     long "max-retries"
  <> short 'r'
  <> metavar "MAX_RETRIES"
  <> help "Maximum number of times to retry failed request"

debugMaxBooks' :: Opt.Parser Int
debugMaxBooks' = option auto $
     long "max-books"
  <> metavar "MAX_BOOKS"
  <> help "(DEBUG) Limit number of fetched orderbooks"

disableVenue :: Opt.Parser Venues.AnyVenue
disableVenue = option parseVenue $
     long "disable-venue"
  <> metavar "VENUE_NAME"
  <> help ("Don't download orderbooks for this specific venue. Supported venues: " <> supportedVenues)

parseVenue :: Opt.ReadM Venues.AnyVenue
parseVenue = do
  str' <- Opt.readerAsk
  case Venues.venueLookup (T.pack str') of
    Just anyVenue -> pure anyVenue
    Nothing -> readerError $
      unwords
        [ "Unknown venue"
        , show str' <> "."
        , "Supported venues:"
        , supportedVenues
        ]

supportedVenues :: String
supportedVenues =
  T.unpack (T.intercalate ", " Venues.allVenuesText)