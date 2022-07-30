module SimpleClient where

import           Control.Monad                   (void)
import           Control.Monad.IO.Class          (MonadIO (liftIO))
import           Invest.Client
import           Invest.Service.Instruments      (shares)
import           Invest.Service.MarketData
import           Proto.Invest.Instruments
import qualified Proto.Invest.Instruments_Fields as I
import           Proto.Invest.Marketdata
import qualified Proto.Invest.Marketdata_Fields  as MD

getBaseShares :: GrpcClient -> GrpcIO [Share]
getBaseShares gc = shares gc (defMessage & I.instrumentStatus .~ INSTRUMENT_STATUS_BASE)

getSharesLastPrices :: GrpcClient -> [Share] -> GrpcIO [LastPrice]
getSharesLastPrices gc shares = toLastPrices (map (^. I.figi) shares)
    where toLastPrices figis = getLastPrices gc (defMessage & MD.figi .~ figis)

-- Gets base shares prices and prints the first one
runSimpleClient :: IO ()
runSimpleClient = void . runExceptT $ client <#> getBaseShares #>> getSharesLastPrices #> (liftIO . print . head)
    where client = initGrpcClient $ ClientConfig {
        token = "your_token",
        appName = Just "your_app_name"
    }
