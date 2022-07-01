module Invest.Service.MarketData(
    getCandles,
    getLastPrices,
    getOrderBook,
    getTradingStatus,
    getLastTrades
) where

import           Network.GRPC.Client            (RawReply)
import           Network.GRPC.Client.Helpers    (GrpcClient, rawUnary)
import           Network.GRPC.HTTP2.ProtoLens   (RPC (..))
import           Network.HTTP2.Client           (ClientIO, TooMuchConcurrency)
import qualified Proto.Invest.Marketdata        as MD
import qualified Proto.Invest.Marketdata_Fields as MD

getCandles
    :: GrpcClient
    -> MD.GetCandlesRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply MD.GetCandlesResponse))
getCandles = rawUnary (RPC :: RPC MD.MarketDataService "getCandles")

getLastPrices
    :: GrpcClient
    -> MD.GetLastPricesRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply MD.GetLastPricesResponse))
getLastPrices = rawUnary (RPC :: RPC MD.MarketDataService "getLastPrices")

getOrderBook
    :: GrpcClient
    -> MD.GetOrderBookRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply MD.GetOrderBookResponse))
getOrderBook = rawUnary (RPC :: RPC MD.MarketDataService "getOrderBook")

getTradingStatus
    :: GrpcClient
    -> MD.GetTradingStatusRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply MD.GetTradingStatusResponse))
getTradingStatus = rawUnary (RPC :: RPC MD.MarketDataService "getTradingStatus")

getLastTrades
    :: GrpcClient
    -> MD.GetLastTradesRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply MD.GetLastTradesResponse))
getLastTrades = rawUnary (RPC :: RPC MD.MarketDataService "getLastTrades")
