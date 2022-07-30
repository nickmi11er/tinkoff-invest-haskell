module Main where

import           MarketDataStreamClient (runMarketDataStreamClient)
import           SandBoxClient          (runSandBoxClient)
import           SimpleClient           (runSimpleClient)

main :: IO ()
main = do
    print "Start simple client..."
    runSimpleClient
    print "Start sandbox client..."
    runSandBoxClient
    print "Start stream client..."
    runMarketDataStreamClient
