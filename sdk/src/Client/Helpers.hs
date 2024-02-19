{-# LANGUAGE
    TupleSections
  #-}
module Client.Helpers
  ( ChanFlow (..)
  , GrpcClient
  , GrpcIO
  , runGrpc
  , runUnary
  , runUnary_
  , (#>)
  , (#>>)
  , (<#>)
  ) where

import           Control.Exception           (IOException)
import           Control.Monad.Except        (ExceptT, runExceptT, throwError)
import           Control.Monad.Trans         (lift)
import           Network.GRPC.Client         (RawReply)
import           Network.GRPC.Client.Helpers (GrpcClient, rawUnary)
import           Network.GRPC.HTTP2.Encoding (GRPCInput, GRPCOutput)
import           Network.HTTP2.Client        (ClientIO, TooMuchConcurrency)

type GrpcIO a = ExceptT IOException IO a

type GrpcContext a = (GrpcClient, a)

data ChanFlow = Next | Break

runGrpc ∷ ClientIO (Either TooMuchConcurrency (RawReply a)) -> GrpcIO a
runGrpc f = lift $ runExceptT f >>= \case
  Right (Right (Right (_, _, Right res))) -> pure res
  Right (Right (Right (_, _, Left err)))  -> throwError $ userError err
  Right (Right (Left err))                -> throwError . userError . show $ err
  Right (Left err)                        -> throwError . userError . show $ err
  Left err                                -> throwError . userError . show $ err

runUnary ∷ (GRPCInput r i,
            GRPCOutput r a) =>
  r -> GrpcClient -> i -> GrpcIO a
runUnary rpc client req = runGrpc $ rawUnary rpc client req

runUnary_ ∷ (GRPCInput r i,
             GRPCOutput r a) =>
  (a -> b) -> r -> GrpcClient -> i -> ExceptT IOException IO b
runUnary_ m rpc gc req = fmap m (runUnary rpc gc req)

(<#>) ∷ GrpcIO GrpcClient -> (GrpcClient -> GrpcIO a) -> GrpcIO (GrpcContext a)
(<#>) clIO f = clIO >>= \client -> (client,) <$> f client

(#>>) ∷ GrpcIO (GrpcContext a) -> (GrpcClient -> a -> GrpcIO b) -> GrpcIO (GrpcContext b)
(#>>) ctx f = ctx >>= \(cl, a) -> (cl,) <$> f cl a

(#>) ∷ GrpcIO (GrpcContext a) -> (a -> GrpcIO b) -> GrpcIO b
(#>) ctx f = ctx >>= \(_, val) -> f val
