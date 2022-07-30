module Invest.Service.Users(
    getAccounts,
    getMarginAttributes,
    getUserTariff,
    getInfo
) where

import           Control.Lens                 ((&), (.~), (^.))
import           Data.ProtoLens.Message       (defMessage)
import           Data.Text                    as T
import           Invest.Client.Helpers        (GrpcIO, runUnary, runUnary_)
import           Network.GRPC.Client          (RawReply)
import           Network.GRPC.Client.Helpers  (GrpcClient)
import           Network.GRPC.HTTP2.ProtoLens (RPC (..))
import           Network.HTTP2.Client         (ClientIO, TooMuchConcurrency)
import           Proto.Invest.Users
import qualified Proto.Invest.Users_Fields    as U

getAccounts :: GrpcClient -> GrpcIO [Account]
getAccounts gc = runUnary_ (^. U.accounts) (RPC :: RPC UsersService "getAccounts") gc defMessage

getMarginAttributes :: GrpcClient -> String -> GrpcIO GetMarginAttributesResponse
getMarginAttributes gc accountId = runUnary (RPC :: RPC UsersService "getMarginAttributes") gc message
    where message = defMessage & U.accountId .~ T.pack accountId

getUserTariff :: GrpcClient -> GrpcIO GetUserTariffResponse
getUserTariff gc = runUnary (RPC :: RPC UsersService "getUserTariff") gc defMessage

getInfo :: GrpcClient -> GrpcIO GetInfoResponse
getInfo gc = runUnary (RPC :: RPC UsersService "getInfo") gc defMessage
