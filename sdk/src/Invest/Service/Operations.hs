module Invest.Service.Operations(
    getOperations,
    getPortfolio,
    getPositions,
    getWithdrawLimits,
    getBrokerReport,
    getDividendsForeignIssuer
) where

import           Data.ProtoLens.Message       (defMessage)
import           Network.GRPC.Client          (RawReply)
import           Network.GRPC.Client.Helpers  (GrpcClient, rawUnary)
import           Network.GRPC.HTTP2.ProtoLens (RPC (..))
import           Network.HTTP2.Client         (ClientIO, TooMuchConcurrency)
import           Proto.Invest.Operations      as Operations

getOperations
    :: GrpcClient
    -> Operations.OperationsRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply Operations.OperationsResponse))
getOperations = rawUnary (RPC :: RPC Operations.OperationsService "getOperations")

getPortfolio
    :: GrpcClient
    -> Operations.PortfolioRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply Operations.PortfolioResponse))
getPortfolio = rawUnary (RPC :: RPC Operations.OperationsService "getPortfolio")

getPositions
    :: GrpcClient
    -> Operations.PositionsRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply Operations.PositionsResponse))
getPositions = rawUnary (RPC :: RPC Operations.OperationsService "getPositions")

getWithdrawLimits
    :: GrpcClient
    -> Operations.WithdrawLimitsRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply Operations.WithdrawLimitsResponse))
getWithdrawLimits = rawUnary (RPC :: RPC Operations.OperationsService "getWithdrawLimits")

getBrokerReport
    :: GrpcClient
    -> Operations.BrokerReportRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply Operations.BrokerReportResponse))
getBrokerReport = rawUnary (RPC :: RPC Operations.OperationsService "getBrokerReport")

getDividendsForeignIssuer
    :: GrpcClient
    -> Operations.GetDividendsForeignIssuerRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply Operations.GetDividendsForeignIssuerResponse))
getDividendsForeignIssuer = rawUnary (RPC :: RPC Operations.OperationsService "getDividendsForeignIssuer")
