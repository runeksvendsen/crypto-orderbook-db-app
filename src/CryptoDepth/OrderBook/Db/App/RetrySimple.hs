{-# LANGUAGE RecordWildCards #-}
{-
    Temporary workaround the fact that "CryptoVenues.Fetch.EnumMarkets.marketList"
    is not retried yet (nor rate-limited).
-}
{-# LANGUAGE OverloadedStrings #-}
module CryptoDepth.OrderBook.Db.App.RetrySimple
( rateLimitRetrySimple
, Time.Second
)
where

import qualified CryptoVenues.Types.Error                   as AppMErr
import qualified CryptoVenues.Types.AppM.Internal           as IAppM

import           Control.Monad.IO.Class                     (liftIO)
import           Protolude.Conv                             (toS)
import qualified Data.Text                                  as T
import qualified Control.Retry                              as Re
import qualified Control.Logging                            as Log
import qualified Data.Time.Units                            as Time
import qualified Control.RateLimit                          as Lim
import qualified Control.Monad.Reader                       as R


-- | Rate-limiting and retrying for any AppM action
rateLimitRetrySimple
    :: (Time.TimeUnit t)
    => t
    -> IAppM.AppM IO a
    -> IAppM.AppM IO (IAppM.AppM IO a)
rateLimitRetrySimple limit appM = do
    maxRetries <- R.asks IAppM.cfgNumMaxRetries
    cfg <- R.ask
    let appMIO = IAppM.runAppM cfg appM
    fun <- liftIO $ Lim.rateLimitExecution limit $ \_ ->
        Re.retrying (retryPolicy maxRetries) doRetry (const appMIO)
    return $ liftIO (fun ()) >>= IAppM.throwLeft

retryPolicy
    :: Word
    -> Re.RetryPolicyM IO
retryPolicy numMaxRetries =
   Re.fullJitterBackoff backoffMicroSecs
   <> Re.limitRetries (fromIntegral numMaxRetries)
      where backoffMicroSecs = fromIntegral $ Time.toMicroseconds (1 :: Time.Second)

-- NB: copy/pasted from "CryptoVenues.Fetch.MarketBook"
doRetry
    :: Re.RetryStatus
    -> Either AppMErr.Error a
    -> IO Bool
doRetry _                  (Right _)  = return False
doRetry Re.RetryStatus{..} (Left err) = do
    let retrying = shouldRetry (AppMErr.toRetryAction err)
        retryStr = if retrying then "Retrying " else "Not retrying "
        attempt  = " (attempt " <> show' rsIterNumber <> ")"
        logFun = if retrying then Log.warnS else logErrorS
    liftIO $ logFun ("Fetch" <> attempt) $ retryStr <> "failed request: " <> show' err
    return retrying
  where
    show' :: Show a => a -> T.Text
    show' = toS . show
    logErrorS :: T.Text -> T.Text -> IO ()
    logErrorS = Log.loggingLogger Log.LevelError
    shouldRetry :: Re.RetryAction -> Bool
    shouldRetry Re.DontRetry = False
    shouldRetry Re.ConsultPolicy = True
    shouldRetry (Re.ConsultPolicyOverrideDelay _) = True
