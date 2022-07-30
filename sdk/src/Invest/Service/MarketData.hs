module Invest.Service.MarketData(
    getCandles,
    getLastPrices,
    getOrderBook,
    getTradingStatus,
    getLastTrades
) where

import           Control.Lens                   ((^.))
import           Invest.Client.Helpers          (GrpcClient, GrpcIO, runUnary,
                                                 runUnary_)
import           Network.GRPC.HTTP2.ProtoLens   (RPC (..))
import           Proto.Invest.Marketdata
import qualified Proto.Invest.Marketdata_Fields as MD

getCandles :: GrpcClient -> GetCandlesRequest -> GrpcIO [HistoricCandle]
getCandles = runUnary_ (^. MD.candles) (RPC :: RPC MarketDataService "getCandles")

getLastPrices :: GrpcClient -> GetLastPricesRequest -> GrpcIO [LastPrice]
getLastPrices = runUnary_ (^. MD.lastPrices) (RPC :: RPC MarketDataService "getLastPrices")

getOrderBook :: GrpcClient -> GetOrderBookRequest -> GrpcIO GetOrderBookResponse
getOrderBook = runUnary (RPC :: RPC MarketDataService "getOrderBook")

getTradingStatus :: GrpcClient -> GetTradingStatusRequest -> GrpcIO GetTradingStatusResponse
getTradingStatus = runUnary (RPC :: RPC MarketDataService "getTradingStatus")

getLastTrades :: GrpcClient -> GetLastTradesRequest -> GrpcIO [Trade]
getLastTrades = runUnary_ (^. MD.trades) (RPC :: RPC MarketDataService "getLastTrades")
