module Invest.Service.Operations(
    getOperations,
    getPortfolio,
    getPositions,
    getWithdrawLimits,
    getBrokerReport,
    getDividendsForeignIssuer
) where

import           Control.Lens                   ((^.))
import           Data.ProtoLens.Message         (defMessage)
import           Invest.Client.Helpers          (GrpcClient, GrpcIO, runUnary,
                                                 runUnary_)
import           Network.GRPC.HTTP2.ProtoLens   (RPC (..))
import           Proto.Invest.Operations
import qualified Proto.Invest.Operations_Fields as O

getOperations :: GrpcClient -> OperationsRequest -> GrpcIO [Operation]
getOperations = runUnary_ (^. O.operations) (RPC :: RPC OperationsService "getOperations")

getPortfolio :: GrpcClient -> PortfolioRequest -> GrpcIO PortfolioResponse
getPortfolio = runUnary (RPC :: RPC OperationsService "getPortfolio")

getPositions :: GrpcClient -> PositionsRequest -> GrpcIO PositionsResponse
getPositions = runUnary (RPC :: RPC OperationsService "getPositions")

getWithdrawLimits :: GrpcClient -> WithdrawLimitsRequest -> GrpcIO WithdrawLimitsResponse
getWithdrawLimits = runUnary (RPC :: RPC OperationsService "getWithdrawLimits")

getBrokerReport :: GrpcClient -> BrokerReportRequest -> GrpcIO BrokerReportResponse
getBrokerReport = runUnary (RPC :: RPC OperationsService "getBrokerReport")

getDividendsForeignIssuer :: GrpcClient -> GetDividendsForeignIssuerRequest -> GrpcIO GetDividendsForeignIssuerResponse
getDividendsForeignIssuer = runUnary (RPC :: RPC OperationsService "getDividendsForeignIssuer")

getOperationsByCursor :: GrpcClient -> GetOperationsByCursorRequest -> GrpcIO GetOperationsByCursorResponse
getOperationsByCursor = runUnary (RPC :: RPC OperationsService "getOperationsByCursor")
