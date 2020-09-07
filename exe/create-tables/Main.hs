{-# LANGUAGE OverloadedStrings #-}
module Main
( main
) where

import qualified CryptoDepth.OrderBook.Db.Run.CreateTable   as CT

import qualified Options.Applicative                        as Opt
import qualified Data.ByteString                            as BS
import qualified Database.Beam.Postgres                     as Postgres
import qualified Database.Beam.Migrate.Types                as Migrate
import qualified Control.Logging                            as Log


main :: IO ()
main = withLogging $ do
    connString <- Opt.execParser opts
    conn <- Postgres.connectPostgreSQL connString
    checkedDb <- CT.createTables conn
    logInfoS "MAIN" ("Created database" :: String)
    logInfoS "MAIN" $ show (Migrate.collectChecks checkedDb)
  where
    logInfoS = Log.loggingLogger Log.LevelInfo

opts :: Opt.ParserInfo BS.ByteString
opts =
    Opt.info connString' $
       Opt.fullDesc
    <> Opt.header "Write database tables to database"

-- NB: copied from ../app/Options.hs
-- TODO: figure out how to share between two executables
connString' :: Opt.Parser BS.ByteString
connString' = Opt.strOption $
     Opt.long "db-conn"
  <> Opt.short 'c'
  <> Opt.metavar "CONNSTRING"
  <> Opt.help "Database connection string (libpq format)"

-- NB: copied from ../app/Main.hs
-- TODO: figure out how to share between two executables
withLogging :: IO a -> IO a
withLogging ioa = Log.withStderrLogging $ do
    Log.setLogLevel logLevel
    Log.setLogTimeFormat "%T.%3q"
    ioa
  where
    -- TODO: don't hardcode
    logLevel :: Log.LogLevel
    logLevel = Log.LevelDebug
