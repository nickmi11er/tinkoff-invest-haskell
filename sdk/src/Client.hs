{-# LANGUAGE
    TupleSections
  #-}

module Client
  ( ClientConfig (..)
  , GrpcClient
  , GrpcIO
  , defMessage
  , initGrpcClient
  , liftIO
  , runExceptT
  , runGrpc
  , simpleClientConfig
  , (#>)
  , (#>>)
  , (&)
  , (.~)
  , (<#>)
  , (^.)
  ) where

import           Client.Helpers
import           Control.Lens                ((&), (.~), (^.))
import           Control.Monad.Except        (runExceptT, throwError)
import           Control.Monad.IO.Class
import           Control.Monad.Trans         (lift)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as BC (pack)
import           Data.Maybe                  (maybeToList)
import           Data.ProtoLens.Message      (defMessage)
import           Network.GRPC.Client         (uncompressed)
import           Network.GRPC.Client.Helpers (GrpcClientConfig (_grpcClientConfigCompression, _grpcClientConfigHeaders),
                                              grpcClientConfigSimple, setupGrpcClient)
import           Network.HTTP2.Client        (runClientIO)

data ClientConfig
  = ClientConfig
      { token   :: String
      , appName :: Maybe String
      }

simpleClientConfig ∷ String -> ClientConfig
simpleClientConfig myToken = ClientConfig { token = myToken, appName = Nothing }

initGrpcClient ∷ ClientConfig -> GrpcIO GrpcClient
initGrpcClient cf = lift $ runClientIO (setupGrpcClient . prepareGrpcConfig $ cf) >>= \case
  Right client -> pure client
  Left err     -> throwError . userError . show $ err

prepareGrpcConfig ∷ ClientConfig -> GrpcClientConfig
prepareGrpcConfig config = (grpcClientConfigSimple "invest-public-api.tinkoff.ru" 443 True) {
  _grpcClientConfigHeaders = apiHeaders config,
  _grpcClientConfigCompression = uncompressed
}

apiHeaders ∷ ClientConfig -> [(ByteString, ByteString)]
apiHeaders config =
  maybeToList appNameH ++ [tokenH]
  where
    tokenH = (BC.pack "Authorization", BC.pack ("Bearer " ++ token config))
    appNameH = (BC.pack "x-app-name",) . BC.pack <$> appName config
