module Invest.Service.Users(
    getAcccounts,
    getMarginAttributes,
    getUserTariff,
    getInfo
) where

import           Control.Lens                 ((&), (.~))
import           Data.ProtoLens.Message       (defMessage)
import           Data.Text                    as T
import           Network.GRPC.Client          (RawReply)
import           Network.GRPC.Client.Helpers  (GrpcClient, rawUnary)
import           Network.GRPC.HTTP2.ProtoLens (RPC (..))
import           Network.HTTP2.Client         (ClientIO, TooMuchConcurrency)
import qualified Proto.Invest.Users           as U
import qualified Proto.Invest.Users_Fields    as U

getAcccounts
    :: GrpcClient
    -> ClientIO (Either TooMuchConcurrency (RawReply U.GetAccountsResponse))
getAcccounts gc = rawUnary (RPC :: RPC U.UsersService "getAccounts") gc defMessage

getMarginAttributes
    :: GrpcClient
    -> String -- accountIds
    -> ClientIO (Either TooMuchConcurrency (RawReply U.GetMarginAttributesResponse))
getMarginAttributes gc accountId = rawUnary (RPC :: RPC U.UsersService "getMarginAttributes") gc message
    where message = defMessage & U.accountId .~ T.pack accountId

getUserTariff
    :: GrpcClient
    -> ClientIO (Either TooMuchConcurrency (RawReply U.GetUserTariffResponse))
getUserTariff gc = rawUnary (RPC :: RPC U.UsersService "getUserTariff") gc defMessage

getInfo
    :: GrpcClient
    -> ClientIO (Either TooMuchConcurrency (RawReply U.GetInfoResponse))
getInfo gc = rawUnary (RPC :: RPC U.UsersService "getInfo") gc defMessage
