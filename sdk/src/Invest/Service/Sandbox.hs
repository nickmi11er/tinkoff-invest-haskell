module Invest.Service.Sandbox(
    openSandboxAccount,
    getSandboxAccounts,
    closeSandboxAccount,
    postSandboxOrder,
    getSandboxOrders,
    cancelSandboxOrder,
    getSandboxOrderState,
    getSandboxPositions,
    getSandboxOperations,
    getSandboxPortfolio,
    sandboxPayIn
) where

import           Data.ProtoLens.Message       (defMessage)
import           Network.GRPC.Client          (RawReply)
import           Network.GRPC.Client.Helpers  (GrpcClient, rawUnary)
import           Network.GRPC.HTTP2.ProtoLens (RPC (..))
import           Network.HTTP2.Client         (ClientIO, TooMuchConcurrency)
import           Proto.Invest.Operations      as Operations
import           Proto.Invest.Orders          as Orders
import           Proto.Invest.Sandbox         as Sandbox
import           Proto.Invest.Users           as Users

openSandboxAccount
    :: GrpcClient
    -> ClientIO (Either TooMuchConcurrency (RawReply Sandbox.OpenSandboxAccountResponse))
openSandboxAccount gc
    = rawUnary (RPC :: RPC Sandbox.SandboxService "openSandboxAccount") gc defMessage

getSandboxAccounts
    :: GrpcClient
    -> ClientIO (Either TooMuchConcurrency (RawReply Users.GetAccountsResponse))
getSandboxAccounts gc
    = rawUnary (RPC :: RPC Sandbox.SandboxService "getSandboxAccounts") gc defMessage

closeSandboxAccount
    :: GrpcClient
    -> Sandbox.CloseSandboxAccountRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply Sandbox.CloseSandboxAccountResponse))
closeSandboxAccount = rawUnary (RPC :: RPC Sandbox.SandboxService "closeSandboxAccount")

postSandboxOrder
    :: GrpcClient
    -> Orders.PostOrderRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply Orders.PostOrderResponse))
postSandboxOrder = rawUnary (RPC :: RPC Sandbox.SandboxService "postSandboxOrder")

getSandboxOrders
    :: GrpcClient
    -> Orders.GetOrdersRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply Orders.GetOrdersResponse))
getSandboxOrders = rawUnary (RPC :: RPC Sandbox.SandboxService "getSandboxOrders")

cancelSandboxOrder
    :: GrpcClient
    -> Orders.CancelOrderRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply Orders.CancelOrderResponse))
cancelSandboxOrder = rawUnary (RPC :: RPC Sandbox.SandboxService "cancelSandboxOrder")

getSandboxOrderState
    :: GrpcClient
    -> Orders.GetOrderStateRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply Orders.OrderState))
getSandboxOrderState = rawUnary (RPC :: RPC Sandbox.SandboxService "getSandboxOrderState")

getSandboxPositions
    :: GrpcClient
    -> Operations.PositionsRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply Operations.PositionsResponse))
getSandboxPositions = rawUnary (RPC :: RPC Sandbox.SandboxService "getSandboxPositions")

getSandboxOperations
    :: GrpcClient
    -> Operations.OperationsRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply Operations.OperationsResponse))
getSandboxOperations = rawUnary (RPC :: RPC Sandbox.SandboxService "getSandboxOperations")

getSandboxPortfolio
    :: GrpcClient
    -> Operations.PortfolioRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply Operations.PortfolioResponse))
getSandboxPortfolio = rawUnary (RPC :: RPC Sandbox.SandboxService "getSandboxPortfolio")

sandboxPayIn
    :: GrpcClient
    -> Sandbox.SandboxPayInRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply Sandbox.SandboxPayInResponse))
sandboxPayIn = rawUnary (RPC :: RPC Sandbox.SandboxService "sandboxPayIn")
