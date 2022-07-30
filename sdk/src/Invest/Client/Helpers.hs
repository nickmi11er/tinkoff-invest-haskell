{-# LANGUAGE TupleSections #-}
module Invest.Client.Helpers (
      runGrpc
    , runUnary
    , runUnary_
    , GrpcIO
    , GrpcClient
    , (<#>), (#>>), (#>)
    , ChanFlow(..)
) where

import           Control.Exception           (Exception, IOException,
                                              SomeException (SomeException))
import           Control.Monad.Except        (ExceptT, lift, runExceptT,
                                              throwError)
import           Data.Text                   as T (Text, pack)
import           Network.GRPC.Client         (RawReply)
import           Network.GRPC.Client.Helpers (GrpcClient, rawUnary)
import           Network.HTTP2.Client        (ClientIO, TooMuchConcurrency,
                                              runClientIO)

type GrpcIO a = ExceptT IOException IO a

type GrpcContext a = (GrpcClient, a)

data ChanFlow = Next | Break

runGrpc :: ClientIO (Either TooMuchConcurrency (RawReply a)) -> GrpcIO a
runGrpc f = lift $ runExceptT f >>= \case
    Right (Right (Right (_, _, Right res))) -> pure res
    Right (Right (Right (_, _, Left err))) -> throwError $ userError err
    Right (Right (Left err)) -> throwError . userError . show $ err
    Right (Left err) -> throwError . userError . show $ err
    Left err -> throwError . userError . show $ err

runUnary rpc client req = runGrpc $ rawUnary rpc client req

runUnary_ m rpc gc req = fmap m (runUnary rpc gc req)

(<#>) :: GrpcIO GrpcClient -> (GrpcClient -> GrpcIO a) -> GrpcIO (GrpcContext a)
(<#>) clIO f = clIO >>= \client -> (client,) <$> f client

(#>>) :: GrpcIO (GrpcContext a) -> (GrpcClient -> a -> GrpcIO b) -> GrpcIO (GrpcContext b)
(#>>) ctx f = ctx >>= \(cl, a) -> (cl,) <$> f cl a

(#>) :: GrpcIO (GrpcContext a) -> (a -> GrpcIO b) -> GrpcIO b
(#>) ctx f = ctx >>= \(_, val) -> f val
