{-# LANGUAGE TupleSections #-}
module Invest.Client(
      ClientConfig(..)
    , simpleClientConfig
    , initGrpcClient
    , (&), (.~), (^.)
    , defMessage
    , runGrpc
    , (<#>), (#>>), (#>)
    , runExceptT
    , GrpcIO
    , GrpcClient
    , liftIO
) where

import           Control.Exception           (throwIO)
import           Control.Lens                ((&), (.~), (^.))
import           Control.Monad.Except        (ExceptT, runExceptT, throwError)
import           Control.Monad.IO.Class
import           Control.Monad.Trans         (lift)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as BC (pack)
import           Data.Maybe                  (maybeToList)
import           Data.ProtoLens.Message      (defMessage)
import           Data.Text                   as T (pack)
import           Invest.Client.Errors        (SDKError (..))
import           Invest.Client.Helpers
import           Network.GRPC.Client         (uncompressed)
import           Network.GRPC.Client.Helpers (GrpcClient,
                                              GrpcClientConfig (_grpcClientConfigCompression, _grpcClientConfigHeaders),
                                              grpcClientConfigSimple,
                                              setupGrpcClient)
import           Network.HTTP2.Client        (ClientIO, runClientIO)

data ClientConfig = ClientConfig { token :: String, appName :: Maybe String }

simpleClientConfig :: String -> ClientConfig
simpleClientConfig token = ClientConfig { token = token, appName = Nothing }

initGrpcClient :: ClientConfig -> GrpcIO GrpcClient
initGrpcClient cf = lift $ runClientIO (setupGrpcClient . prepareGrpcConfig $ cf) >>= \case
    Right client -> pure client
    Left err     -> throwError . userError . show $ err

prepareGrpcConfig :: ClientConfig -> GrpcClientConfig
prepareGrpcConfig config = (grpcClientConfigSimple "invest-public-api.tinkoff.ru" 443 True) {
    _grpcClientConfigHeaders = apiHeaders config,
    _grpcClientConfigCompression = uncompressed
}

apiHeaders :: ClientConfig -> [(ByteString, ByteString)]
apiHeaders config =
    maybeToList appNameH ++ [tokenH]
    where
        tokenH = (BC.pack "Authorization", BC.pack ("Bearer " ++ token config))
        appNameH = (BC.pack "x-app-name",) . BC.pack <$> appName config
