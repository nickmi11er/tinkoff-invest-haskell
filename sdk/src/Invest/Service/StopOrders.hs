module Invest.Service.StopOrders(
    postStopOrder,
    getStopOrders,
    cancelStopOrder
) where

import           Control.Lens                   ((&), (.~))
import           Data.ProtoLens.Message         (defMessage)
import           Data.Text                      as T
import           Network.GRPC.Client            (RawReply)
import           Network.GRPC.Client.Helpers    (GrpcClient, rawUnary)
import           Network.GRPC.HTTP2.ProtoLens   (RPC (..))
import           Network.HTTP2.Client           (ClientIO, TooMuchConcurrency)
import           Proto.Invest.Stoporders        as SO
import           Proto.Invest.Stoporders_Fields as SO

postStopOrder
    :: GrpcClient
    -> SO.PostStopOrderRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply SO.PostStopOrderResponse))
postStopOrder = rawUnary (RPC :: RPC SO.StopOrdersService "postStopOrder")

getStopOrders
    :: GrpcClient
    -> String -- accountId
    -> ClientIO (Either TooMuchConcurrency (RawReply SO.GetStopOrdersResponse))
getStopOrders gc accountId = rawUnary (RPC :: RPC SO.StopOrdersService "getStopOrders") gc message
    where message = defMessage & SO.accountId .~ T.pack accountId

cancelStopOrder
    :: GrpcClient
    -> SO.CancelStopOrderRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply SO.CancelStopOrderResponse))
cancelStopOrder = rawUnary (RPC :: RPC SO.StopOrdersService "cancelStopOrder")
