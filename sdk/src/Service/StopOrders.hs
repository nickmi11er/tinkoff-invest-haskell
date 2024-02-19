module Service.StopOrders
  ( cancelStopOrder
  , getStopOrders
  , postStopOrder
  ) where

import           Client.Helpers                  (GrpcClient, GrpcIO, runUnary, runUnary_)
import           Control.Lens                    ((&), (.~), (^.))
import           Data.ProtoLens.Message          (defMessage)
import           Data.Text                       as T
import           Network.GRPC.HTTP2.ProtoLens    (RPC (..))
import qualified Proto.Common_Fields             as C
import           Proto.Google.Protobuf.Timestamp
import           Proto.Stoporders
import qualified Proto.Stoporders_Fields         as SO

postStopOrder ∷ GrpcClient -> PostStopOrderRequest -> GrpcIO Text
postStopOrder = runUnary_ (^. SO.stopOrderId) (RPC :: RPC StopOrdersService "postStopOrder")

getStopOrders ∷ GrpcClient -> Text -> GrpcIO [StopOrder]
getStopOrders gc accountId = runUnary_ (^. SO.stopOrders) (RPC :: RPC StopOrdersService "getStopOrders") gc message
  where message = defMessage & SO.accountId .~ accountId

cancelStopOrder ∷ GrpcClient -> CancelStopOrderRequest -> GrpcIO Timestamp
cancelStopOrder = runUnary_ (^. C.time) (RPC :: RPC StopOrdersService "cancelStopOrder")
