module Main where

import           Control.Concurrent              (forkIO, threadDelay)
import           Control.Lens                    ((&), (.~), (^.))
import           Control.Monad.IO.Class          (MonadIO (liftIO))
import           Data.Char
import           Data.ProtoLens.Message          (defMessage)
import           Invest.Client
import           Invest.Client.Helpers
import qualified Invest.Service.Instruments      as I
import qualified Invest.Service.MarketData       as MD
import qualified Invest.Service.MarketDataStream as MDS
import           Network.GRPC.Client.Helpers     (GrpcClient)
import           Network.HTTP2.Client            (ClientIO, TooMuchConcurrency,
                                                  runClientIO)
import qualified Proto.Invest.Instruments        as I
import qualified Proto.Invest.Instruments_Fields as I
import qualified Proto.Invest.Marketdata         as MD
import qualified Proto.Invest.Marketdata_Fields  as MD

getBaseShares :: GrpcClient -> IO [I.Share]
getBaseShares gc = (^. I.instruments) <$> (liftIO . runGrpc $ I.shares gc req)
  where req = defMessage & I.instrumentStatus .~ I.INSTRUMENT_STATUS_BASE

getSharesLastPrices :: GrpcClient -> [I.Share] -> IO [MD.LastPrice]
getSharesLastPrices gc shares = toLastPrices (map (^. I.figi) shares)
  where
    toLastPrices figiArr = (^. MD.lastPrices) <$> runGrpc (MD.getLastPrices gc (defMessage & MD.figi .~ figiArr))

listenMarketData :: GrpcClient -> IO ()
listenMarketData client = do
  res <- liftIO . runClientIO $ MDS.marketDataServerSideStream client $ \response ->
    print response
  return ()

main :: IO ()
main = do
  let config = ClientConfig {
    token = "your_token",
    appName = Just "YourAppName"
  }
  client <- initGrpcClient config
  shares <- getBaseShares client
  shareesLastPrices <- getSharesLastPrices client shares
  mapM_ print shareesLastPrices
  -- forkIO $ listenMarketData client
  -- threadDelay 5000000
  putStrLn "END!"
