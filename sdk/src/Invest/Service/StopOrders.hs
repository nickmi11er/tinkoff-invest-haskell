module Invest.Service.StopOrders(
    postStopOrder,
    getStopOrders,
    cancelStopOrder
) where

import           Control.Lens                    ((&), (.~), (^.))
import           Data.ProtoLens.Message          (defMessage)
import           Data.Text                       as T
import           Invest.Client.Helpers           (GrpcClient, GrpcIO, runUnary,
                                                  runUnary_)
import           Network.GRPC.HTTP2.ProtoLens    (RPC (..))
import           Proto.Google.Protobuf.Timestamp
import qualified Proto.Invest.Common_Fields      as C
import           Proto.Invest.Stoporders
import qualified Proto.Invest.Stoporders_Fields  as SO

postStopOrder :: GrpcClient -> PostStopOrderRequest -> GrpcIO Text
postStopOrder = runUnary_ (^. SO.stopOrderId) (RPC :: RPC StopOrdersService "postStopOrder")

getStopOrders :: GrpcClient -> Text -> GrpcIO [StopOrder]
getStopOrders gc accountId = runUnary_ (^. SO.stopOrders) (RPC :: RPC StopOrdersService "getStopOrders") gc message
    where message = defMessage & SO.accountId .~ accountId

cancelStopOrder :: GrpcClient -> CancelStopOrderRequest -> GrpcIO Timestamp
cancelStopOrder = runUnary_ (^. C.time) (RPC :: RPC StopOrdersService "cancelStopOrder")
