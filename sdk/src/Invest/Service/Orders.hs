module Invest.Service.Orders(
    postOrder,
    cancelOrder,
    getOrderState,
    getOrders
) where

import           Control.Lens                    ((^.))
import           Data.ProtoLens.Message          (defMessage)
import           Invest.Client.Helpers           (GrpcClient, GrpcIO, runUnary,
                                                  runUnary_)
import           Network.GRPC.HTTP2.ProtoLens    (RPC (..))
import           Proto.Google.Protobuf.Timestamp
import qualified Proto.Invest.Common_Fields      as C
import           Proto.Invest.Orders
import qualified Proto.Invest.Orders_Fields      as O

postOrder :: GrpcClient -> PostOrderRequest -> GrpcIO PostOrderResponse
postOrder = runUnary (RPC :: RPC OrdersService "postOrder")

cancelOrder :: GrpcClient -> CancelOrderRequest -> GrpcIO Timestamp
cancelOrder = runUnary_ (^. C.time) (RPC :: RPC OrdersService "cancelOrder")

getOrderState :: GrpcClient -> GetOrderStateRequest -> GrpcIO OrderState
getOrderState = runUnary (RPC :: RPC OrdersService "getOrderState")

getOrders :: GrpcClient -> GetOrdersRequest -> GrpcIO [OrderState]
getOrders = runUnary_ (^. O.orders) (RPC :: RPC OrdersService "getOrders")

replaceOrder :: GrpcClient -> ReplaceOrderRequest -> GrpcIO PostOrderResponse
replaceOrder = runUnary (RPC :: RPC OrdersService "replaceOrder")
