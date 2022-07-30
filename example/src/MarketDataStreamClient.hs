{-# LANGUAGE BlockArguments #-}
module MarketDataStreamClient where
import           Control.Concurrent              (threadDelay, forkIO)
import           Invest.Client
import           Invest.Client.Helpers
import           Invest.Service.MarketDataStream
import           Proto.Invest.Marketdata
import qualified Proto.Invest.Marketdata_Fields  as MD

runClient :: ClientConfig -> IO GrpcClient
runClient cnfg = runExceptT (initGrpcClient cnfg) >>= \case
    Left err -> error . show $ err
    Right gc -> pure gc

logResponse :: (Show a) => a -> IO ()
logResponse = liftIO . print . ("Response: " ++) . show

runMarketDataStreamClient :: IO ()
runMarketDataStreamClient = do
    let config = ClientConfig {
        token = "your_token",
        appName = Just "your_app_name"
    }
    client <- runClient config
    stream <- marketDataStream client

    subscribeOrderBook stream "BBG004730RP0" 10 \resp -> case resp ^. MD.maybe'payload of
        Just (MarketDataResponse'SubscribeOrderBookResponse r) -> logResponse r >> pure Next
        Just (MarketDataResponse'Orderbook r) -> logResponse r >> pure Next
        _ -> return Break

    -- close stream after 5 seconds
    forkIO $ threadDelay 5000000 >> close stream
    -- interrupt subscription after 3 seconds
    forkIO $ threadDelay 3000000 >> unsubscribeOrderBook stream "BBG004730RP0" 10
    
    wait stream
