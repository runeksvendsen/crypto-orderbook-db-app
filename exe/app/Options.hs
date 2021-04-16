module Options
( Options(..)
, withArgs
, connString'
)
where

import           Prelude
import qualified Data.ByteString                as BS
import qualified Options.Applicative            as Opt
import           Options.Applicative


withArgs :: (Options -> IO a) -> IO a
withArgs f = do
    args <- Opt.execParser opts
    f args

data Options = Options
    { dbConnString      :: BS.ByteString
    , dbMaxRetries      :: Word
    , fetchMaxRetries   :: Word
    , debugMaxBooks     :: Maybe Int
    }

options :: Opt.Parser Options
options = Options
    <$> connString'
    <*> dbConnRetry'
    <*> fetchMaxRetries'
    <*> optional debugMaxBooks'

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
