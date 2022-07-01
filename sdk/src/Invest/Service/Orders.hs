module Invest.Service.Orders(
    postOrder,
    cancelOrder,
    getOrderState,
    getOrders
) where

import           Data.ProtoLens.Message       (defMessage)
import           Network.GRPC.Client          (RawReply)
import           Network.GRPC.Client.Helpers  (GrpcClient, rawUnary)
import           Network.GRPC.HTTP2.ProtoLens (RPC (..))
import           Network.HTTP2.Client         (ClientIO, TooMuchConcurrency)
import           Proto.Invest.Orders          as Orders

postOrder
    :: GrpcClient
    -> Orders.PostOrderRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply Orders.PostOrderResponse))
postOrder = rawUnary (RPC :: RPC Orders.OrdersService "postOrder")

cancelOrder
    :: GrpcClient
    -> Orders.CancelOrderRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply Orders.CancelOrderResponse))
cancelOrder = rawUnary (RPC :: RPC Orders.OrdersService "cancelOrder")

getOrderState
    :: GrpcClient
    -> Orders.GetOrderStateRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply Orders.OrderState))
getOrderState = rawUnary (RPC :: RPC Orders.OrdersService "getOrderState")

getOrders
    :: GrpcClient
    -> Orders.GetOrdersRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply Orders.GetOrdersResponse))
getOrders = rawUnary (RPC :: RPC Orders.OrdersService "getOrders")
