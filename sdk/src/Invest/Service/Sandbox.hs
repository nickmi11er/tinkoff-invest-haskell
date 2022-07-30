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

import           Control.Lens                    ((&), (.~), (^.))
import           Data.ProtoLens.Message          (defMessage)
import           Data.Text
import           Invest.Client.Helpers           (GrpcClient, GrpcIO, runUnary,
                                                  runUnary_)
import           Network.GRPC.Client             (RawReply)
import           Network.GRPC.HTTP2.ProtoLens    (RPC (..))
import           Proto.Google.Protobuf.Timestamp
import qualified Proto.Invest.Common_Fields      as C
import           Proto.Invest.Operations
import qualified Proto.Invest.Operations_Fields  as OP
import           Proto.Invest.Orders
import qualified Proto.Invest.Orders_Fields      as O
import           Proto.Invest.Sandbox
import qualified Proto.Invest.Sandbox_Fields     as S
import           Proto.Invest.Users
import qualified Proto.Invest.Users_Fields       as U
import Proto.Invest.Common

openSandboxAccount :: GrpcClient -> GrpcIO Text
openSandboxAccount gc = runUnary_ (^. S.accountId) (RPC :: RPC SandboxService "openSandboxAccount") gc defMessage

getSandboxAccounts :: GrpcClient -> GrpcIO [Account]
getSandboxAccounts gc = runUnary_ (^. U.accounts) (RPC :: RPC SandboxService "getSandboxAccounts") gc defMessage

closeSandboxAccount :: GrpcClient -> CloseSandboxAccountRequest -> GrpcIO CloseSandboxAccountResponse
closeSandboxAccount = runUnary (RPC :: RPC SandboxService "closeSandboxAccount")

postSandboxOrder :: GrpcClient -> PostOrderRequest -> GrpcIO PostOrderResponse
postSandboxOrder = runUnary (RPC :: RPC SandboxService "postSandboxOrder")

getSandboxOrders :: GrpcClient -> Text -> GrpcIO [OrderState]
getSandboxOrders gc accountId = runUnary_ (^. O.orders) (RPC :: RPC SandboxService "getSandboxOrders") gc message
    where message = defMessage & O.accountId .~ accountId

cancelSandboxOrder :: GrpcClient -> CancelOrderRequest -> GrpcIO Timestamp
cancelSandboxOrder = runUnary_ (^. C.time) (RPC :: RPC SandboxService "cancelSandboxOrder")

getSandboxOrderState :: GrpcClient -> GetOrderStateRequest -> GrpcIO OrderState
getSandboxOrderState = runUnary (RPC :: RPC SandboxService "getSandboxOrderState")

getSandboxPositions :: GrpcClient -> PositionsRequest -> GrpcIO PositionsResponse
getSandboxPositions = runUnary (RPC :: RPC SandboxService "getSandboxPositions")

getSandboxOperations :: GrpcClient -> OperationsRequest -> GrpcIO [Operation]
getSandboxOperations = runUnary_ (^. OP.operations) (RPC :: RPC SandboxService "getSandboxOperations")

getSandboxPortfolio :: GrpcClient -> PortfolioRequest -> GrpcIO PortfolioResponse
getSandboxPortfolio = runUnary (RPC :: RPC SandboxService "getSandboxPortfolio")

sandboxPayIn :: GrpcClient -> SandboxPayInRequest -> GrpcIO MoneyValue
sandboxPayIn = runUnary_ (^. S.balance) (RPC :: RPC SandboxService "sandboxPayIn")
