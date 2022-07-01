module Invest.Client.Helpers (
    runGrpc
) where

import           Data.Text            as T (Text, pack)
import           Invest.Client.Errors (SDKError (..))
import           Network.GRPC.Client  (RawReply)
import           Network.HTTP2.Client (ClientIO, TooMuchConcurrency,
                                       runClientIO)

runGrpc :: ClientIO (Either TooMuchConcurrency (RawReply a)) -> IO a
runGrpc f = runClientIO f >>= \case
    Right (Right (Right (_, _, Right res))) -> pure res
    Right (Right (Right (_, _, Left err))) -> error . show $ GrpcError (T.pack $ show err)
    Right (Right (Left err)) -> error . show $ GrpcError (T.pack $ show err)
    Right (Left err) -> error . show $ GrpcError (T.pack $ show err)
    Left err -> error . show $ GrpcError (T.pack $ show err)

