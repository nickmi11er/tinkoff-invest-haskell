module Invest.Service.MarketDataStream(
    marketDataServerSideStream
) where

import           Control.Lens                   ((&), (.~), (^.))
import           Control.Monad.IO.Class
import           Data.ProtoLens.Message         (defMessage)
import           Data.Text                      as T
import           Network.GRPC.Client            (BiDiStep (Abort, SendInput, WaitOutput),
                                                 CompressMode (Uncompressed),
                                                 HeaderList (..), RawReply,
                                                 StreamDone (..))
import           Network.GRPC.Client.Helpers    (GrpcClient, rawStreamServer)
import           Network.GRPC.HTTP2.ProtoLens   (RPC (..))
import           Network.HTTP2.Client           (ClientIO, TooMuchConcurrency)
import qualified Proto.Invest.Marketdata        as MD
import qualified Proto.Invest.Marketdata_Fields as MD

marketDataServerSideStream
    :: GrpcClient
    -> (MD.MarketDataResponse -> IO ())
    -- -> MD.MarketDataServerSideStreamRequest
    -> ClientIO (Either TooMuchConcurrency (Maybe MD.MarketDataResponse, HeaderList, HeaderList))
marketDataServerSideStream gc callback
    = rawStreamServer (RPC :: RPC MD.MarketDataStreamService "marketDataServerSideStream") gc Nothing message $ \state _ o -> liftIO $ do
        callback o
        return (Just o)
    where
        instrument = defMessage & MD.figi .~ T.pack "BBG004730RP0" & MD.interval .~ MD.SUBSCRIPTION_INTERVAL_ONE_MINUTE
        message = defMessage &
            MD.subscribeCandlesRequest .~ (defMessage &
                MD.subscriptionAction .~ MD.SUBSCRIPTION_ACTION_SUBSCRIBE &
                MD.waitingClose .~ True &
                MD.instruments .~ [instrument]
            )

-- marketDataStream
--     :: GrpcClient
--     -> (MD.MarketDataRequest -> IO (Maybe MD.MarketDataResponse))
--     -> ClientIO (Either TooMuchConcurrency (Maybe MD.MarketDataRequest))
-- marketDataStream gc req
--     = rawSteppedBidirectional (RPC :: RPC MD.MarketDataStreamService "marketDataStream") gc Nothing $ \case
--         Nothing -> pure (Nothing, WaitOutput (\_ _ o -> pure $ Just o) (\_ a _ -> pure a))
--         Just x -> do
--             accept <- liftIO $ f x
--             case accept of
--                 Nothing -> pure (Nothing, Abort)
--                 Just r  -> pure (Nothing, SendInput Uncompressed r)


-- marketDataStream
--     :: GrpcClient
--     -> MD.MarketDataRequest
--     -> ClientIO (Either TooMuchConcurrency (Maybe MD.MarketDataRequest))
-- marketDataStream gc req
--     = rawStreamClient (RPC :: RPC MD.MarketDataStreamService "marketDataStream") gc Nothing $ \state -> case state of
--         Nothing -> pure (Nothing, Right (Uncompressed, req))
--         Just r -> do
--             accept <- liftIO $ r
--             case accept of
--                 Nothing -> Left StreamDone
--                 Just r1  -> pure (Nothing, SendInput Uncompressed r1)
