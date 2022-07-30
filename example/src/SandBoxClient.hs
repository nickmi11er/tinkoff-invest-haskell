module SandBoxClient where

import           Control.Monad             (void)
import           Control.Monad.IO.Class    (MonadIO (liftIO))
import           Data.Text                 (Text)
import           Invest.Client
import           Invest.Service.Sandbox
import           Proto.Invest.Users_Fields as U

-- Creates new sandbox account if there is no one
getSandBoxAccountId :: GrpcClient -> GrpcIO Text
getSandBoxAccountId gc = getSandboxAccounts gc >>= \case
    []  -> openSandboxAccount gc
    arr -> pure $ head arr ^. U.id

runSandBoxClient :: IO ()
runSandBoxClient = void . runExceptT $ do
    client <- initClient
    accountId <- getSandBoxAccountId client
    orders <- getSandboxOrders client accountId
    liftIO . print $ orders
    where initClient = initGrpcClient $ ClientConfig {
        token = "your_sandbox_token",
        appName = Just "your_sandbox_app_name"
    }
