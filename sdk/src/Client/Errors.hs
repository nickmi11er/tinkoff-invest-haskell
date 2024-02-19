module Client.Errors
  ( SDKError (..)
  ) where

import           Control.Exception (Exception)
import           Data.Text         (Text)

data SDKError
  = ClientSetupError Text
  | GrpcError Text
  deriving (Show)

instance Exception SDKError
