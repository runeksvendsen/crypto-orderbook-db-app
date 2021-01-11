{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main
( main
) where

import           Prelude
import qualified Options
import qualified CryptoDepth.OrderBook.Db.App.Runner    as Runner
import qualified CryptoDepth.OrderBook.Db.App.PqConnect as PqConnect
import           Protolude.Conv                         (toS)
import qualified OrderBook.Types                        as OB
import qualified CryptoDepth.OrderBook.Db.Insert        as Insert
import           CryptoDepth.OrderBook.Db.Insert        (SomeOrderBook)
import qualified CryptoDepth.OrderBook.Db.Monad         as Db
import qualified CryptoDepth.OrderBook.Db.Util          as Util
import qualified CryptoDepth.OrderBook.Db.App.RetrySimple as RS

-- CryptoVenues
import qualified CryptoVenues
import qualified CryptoVenues.Types.ABook               as AB
import qualified CryptoVenues.Types.Market              as CryptoVenues
import qualified CryptoVenues.Fetch.EnumMarkets         as EnumMarkets
import qualified CryptoVenues.Venues                    as Venues
import qualified CryptoVenues.Types.AppM                as AppM
import qualified CryptoVenues.Types.Error               as AppMErr
import           CryptoVenues.Fetch.MarketBook          (fetchMarketBook)

import qualified Database.Beam.Postgres                 as Postgres
import qualified Data.Text                              as T
import qualified Control.Monad.Parallel                 as Par
import qualified Network.HTTP.Client.TLS                as HTTPS
import qualified Network.HTTP.Client                    as HTTP
import qualified Control.Logging                        as Log
import qualified Data.Time.Clock                        as Clock

import           Data.Proxy                             (Proxy(..))
import           Control.Error                          (fromMaybe, lefts, rights)
import           Control.Monad                          (forM, forM_, (<=<))
import           Control.Monad.IO.Class                 (liftIO)
import           Control.Exception                      (bracket)
import           Data.List                              ((\\))


main :: IO Runner.Void
main = Options.withArgs $ \args ->
    withLogging $ do
        -- Test connection on startup.
        -- Program will crash if a connection cannot be established.
        testConnection args
        -- Keep doing the below with a random pause in-between (in specified range)
        Runner.foreverWithPauseRange (22 :: Runner.Hour) (26 :: Runner.Hour) $
            -- Catch all exceptions and log them as an error
            Runner.logSwallowExceptions $
                connectFetchStore args
  where
    testConnection args' =
        withConnection args' (const $ return ())

withConnection
    :: Options.Options
    -> (Postgres.Connection -> IO a)
    -> IO a
withConnection args =
    bracket openConn Postgres.close
  where
    dbConnString = Options.dbConnString args
    dbMaxRetries = Options.dbMaxRetries args
    openConn = do
        logInfoS "DB" ("Connecting to " ++ show dbConnString)
        PqConnect.pgConnectRetry dbMaxRetries dbConnString

-- | Open database connection,
--   fetch orderbooks,
--   write orderbooks to database,
--   close database connection.
--
--  If an error occurs for any venue nothing will be written
--   to the database, and the return value will be 'Runner.NoPause'
--   (indicating that we should retry immediately).
--  Otherwise 'Runner.Pause' is returned.
connectFetchStore :: Options.Options -> IO Runner.PauseAction
connectFetchStore args = do
    bookFetchRunM <- fetchRun (Options.fetchMaxRetries args) (fromMaybe maxBound $ Options.debugMaxBooks args)
    maybe (return Runner.NoPause) connectSaveRun bookFetchRunM
  where
    connectSaveRun bookFetchRun =
        withConnection args $ \conn ->
            -- Don't store empty run
            if length (timeBookList bookFetchRun) > 0
                then storeBooks conn bookFetchRun >> return Runner.Pause
                else logInfoS "MAIN" "Not inserting empty run"  >> return Runner.NoPause

storeBooks :: Postgres.Connection -> BookRun -> IO ()
storeBooks conn BookRun{..} = do
    (runId, bookIdList) <- Db.runDb conn $
        Insert.storeRun runStartTime runEndTime timeBookList
    logInfoS (toS $ show runId)
             ("Inserted " ++ show (length bookIdList) ++ " books")

logInfoS :: T.Text -> String -> IO ()
logInfoS = Log.loggingLogger Log.LevelInfo

-- Nothing = error
fetchRun :: Word -> Word -> IO (Maybe BookRun)
fetchRun maxRetries debugMaxBooks = do
    runStartTime <- Clock.getCurrentTime
    timeBookListM <- fetchBooks maxRetries debugMaxBooks
    case timeBookListM of
        Nothing -> return Nothing
        Just timeBookList -> do
            runEndTime <- Clock.getCurrentTime
            return $ Just $ BookRun runStartTime timeBookList runEndTime

data BookRun = BookRun
    { runStartTime  :: Clock.UTCTime
    , timeBookList  :: [(Clock.UTCTime, SomeOrderBook)]
    , runEndTime    :: Clock.UTCTime
    }

-- TODO: error handling when running "EnumMarkets.marketList"
-- Don't save books unless all venues succeed.
fetchBooks :: Word -> Word -> IO (Maybe [(Clock.UTCTime, SomeOrderBook)])
fetchBooks maxRetries debugMaxBooks = do
    man <- HTTP.newManager HTTPS.tlsManagerSettings
    booksE <- throwErrM $
        AppM.runAppM man maxRetries $ allBooks debugMaxBooks
    -- Log errors
    let errors = lefts booksE
    forM_ errors logFetchError
    -- TODO: sort the books so that the most frequently
    --  occurring markets are fetched first
    if null errors
        then return . Just . concat . sortMostFrequent . rights $ booksE
        else return Nothing
  where
    sortMostFrequent =
        Util.sortByOccurrenceCount (soVenue . snd) (soBaseQuote . snd)
    soVenue (Insert.SomeOrderBook ob) =
        AB.abVenue ob
    soBaseQuote (Insert.SomeOrderBook ob) =
        (AB.abBase ob, AB.abQuote ob)
    logErrorS :: T.Text -> T.Text -> IO ()
    logErrorS = Log.loggingLogger Log.LevelError
    throwErrM ioA =
        ioA >>= either (error . show) return
    logFetchError (AppMErr.Error ctx err) =
        logErrorS (toS $ show ctx) (toS $ show err)

withLogging :: IO a -> IO a
withLogging ioa = Log.withStderrLogging $ do
    Log.setLogLevel logLevel
    Log.setLogTimeFormat "%T.%3q"
    ioa
  where
    -- TODO: don't hardcode
    logLevel :: Log.LogLevel
    logLevel = Log.LevelInfo

-- | Fetch books, in parallel, from all venues.
--
-- Error handling: for any given venue, return either *all* available
--  order books or an error.
allBooks
    :: Word -> AppM.AppM IO [Either AppM.Error [(Clock.UTCTime, SomeOrderBook)]]
allBooks debugMaxBooks = do
    Par.forM Venues.allVenues $ \(CryptoVenues.AnyVenue p) ->
         AppM.evalAppM (map (fmap $ toSomeOrderBook . AB.toABook) <$> venueBooks debugMaxBooks p)
  where
    toSomeOrderBook (AB.ABook ob) = Insert.SomeOrderBook ob

-- | Fetch all books for a given venue
venueBooks
    :: CryptoVenues.MarketBook venue
    => Word
    -> Proxy venue
    -> AppM.AppM IO [(Clock.UTCTime, OB.AnyBook venue)]
venueBooks debugMaxBooks _ = do
    allMarkets <- retrying EnumMarkets.marketList
    let marketList = debugFilterMarkets "USD" debugMaxBooks allMarkets
    forM marketList $ \market -> do
        book <- fetchMarketBook market
        time <- liftIO Clock.getCurrentTime
        return (time, book)
  where
    retrying = id <=< RS.rateLimitRetrySimple (1 :: RS.Second)

debugFilterMarkets numeraire numObLimit allMarkets =
    numeraireLst ++ markets
  where
    btcEth = ["BTC", "ETH"]
    numeraireLst = filter (\mkt -> CryptoVenues.miBase mkt `elem` btcEth && CryptoVenues.miQuote mkt == numeraire) allMarkets
    markets = take (fromIntegral numObLimit - length numeraireLst) (allMarkets \\ numeraireLst)
