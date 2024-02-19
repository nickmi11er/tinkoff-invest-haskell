module Service.Orders
  ( cancelOrder
  , getOrderState
  , getOrders
  , postOrder
  , replaceOrder
  ) where

import           Client.Helpers                  (GrpcClient, GrpcIO, runUnary, runUnary_)

import           Control.Lens                    ((^.))

import           Network.GRPC.HTTP2.ProtoLens    (RPC (..))

import qualified Proto.Common_Fields             as C
import           Proto.Google.Protobuf.Timestamp
import           Proto.Orders
import qualified Proto.Orders_Fields             as O

postOrder ∷ GrpcClient -> PostOrderRequest -> GrpcIO PostOrderResponse
postOrder = runUnary (RPC :: RPC OrdersService "postOrder")

cancelOrder ∷ GrpcClient -> CancelOrderRequest -> GrpcIO Timestamp
cancelOrder = runUnary_ (^. C.time) (RPC :: RPC OrdersService "cancelOrder")

getOrderState ∷ GrpcClient -> GetOrderStateRequest -> GrpcIO OrderState
getOrderState = runUnary (RPC :: RPC OrdersService "getOrderState")

getOrders ∷ GrpcClient -> GetOrdersRequest -> GrpcIO [OrderState]
getOrders = runUnary_ (^. O.orders) (RPC :: RPC OrdersService "getOrders")

replaceOrder ∷ GrpcClient -> ReplaceOrderRequest -> GrpcIO PostOrderResponse
replaceOrder = runUnary (RPC :: RPC OrdersService "replaceOrder")
