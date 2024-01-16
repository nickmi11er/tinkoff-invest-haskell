module Invest.Service.Internal.MarketDataStream(
    requestId,
    responseId,
    producedBy,
    (<@), (@>),
    MDStream,
    MDStreamMonad,
    STRequest(..),
    STResponse(..)
) where

import           Control.Concurrent             (MVar, forkIO)
import           Control.Concurrent             as GHC.Conc.Sync (ThreadId)
import           Control.Concurrent.Chan        (Chan, readChan, writeChan)
import           Control.Exception              (SomeException)
import           Control.Lens                   ((&), (.~), (^.))
import           Control.Monad                  (forever, void)
import           Control.Monad.Except           (throwError)
import           Control.Monad.IO.Class
import           Data.Int                       (Int32)
import           Data.Text                      as T (Text)
import           Invest.Client
import           Invest.Client.Helpers          (ChanFlow (..))
import           Proto.Invest.Marketdata
import           Proto.Invest.Marketdata_Fields

data STRequest  = PostRequest MarketDataRequest | Shutdown
data STResponse = Message MarketDataResponse | StreamStopped | StreamError SomeException

type Closed = MVar ()

type MDStream = (Closed, Chan STRequest, Chan STResponse)

type MDStreamMonad = IO MDStream

data AppFault = BadFault SomeException | SomeOtherAppProblem deriving Show

data SubId =
    Candles   { candlesInsts :: [(Text, SubscriptionInterval)] } -- (figi, interval)
  | OrderBook { orderInsts :: [(Text, Int32)] }                  -- (figi, depth)
  | Trades    { figis :: [Text] }
  | Info      { figis :: [Text] }
  | LastPrice { figis :: [Text] }
  | Unknown
  deriving (Eq, Show)

(<@) :: MDStream -> STRequest -> IO ()
(<@) (_, requests, _) = writeChan requests

(@>) :: MDStream -> (MarketDataResponse -> IO ChanFlow) -> IO ThreadId
(@>) (_, _, responses) callback = forkIO . void $ pollChan responses callback

pollChan :: Chan STResponse -> (MarketDataResponse -> IO ChanFlow) -> IO ()
pollChan chan callback = runExceptT (forever $ (liftIO . readChan $ chan) >>= \case
        Message response -> do
          res <- runExceptT $ liftIO (callback response) >>= \case
            Next  -> return ()
            Break -> throwError SomeOtherAppProblem
          case res of
            Left err -> throwError err
            Right x0 -> return ()
        StreamStopped    -> throwError SomeOtherAppProblem
        StreamError err  -> throwError (BadFault err)
    ) >>= either handleFault pure

handleFault :: AppFault -> IO ()
handleFault _ = return () -- ignore interruption message

requestId :: MarketDataRequest -> SubId
requestId request = case request ^. maybe'payload of
  Just (MarketDataRequest'SubscribeInfoRequest r) -> Info { figis = map (^. figi) $ r ^. instruments }
  Just (MarketDataRequest'SubscribeTradesRequest r) -> Trades { figis = map (^. figi) $ r ^. instruments }
  Just (MarketDataRequest'SubscribeLastPriceRequest r) -> LastPrice { figis = map (^. figi) $ r ^. instruments }
  Just (MarketDataRequest'SubscribeOrderBookRequest r) ->
    OrderBook { orderInsts = map (\i -> (i ^. figi, i ^. depth)) $ r ^. instruments }
  Just (MarketDataRequest'SubscribeCandlesRequest r) ->
    Candles { candlesInsts = map (\i -> (i ^. figi, i ^. interval)) $ r ^. instruments }
  Just (MarketDataRequest'GetMySubscriptions r) -> Unknown -- What is the response?
  Nothing -> Unknown

responseId :: MarketDataResponse -> SubId
responseId response = case response ^.maybe'payload of
  Just (MarketDataResponse'SubscribeCandlesResponse r) ->
    Candles { candlesInsts = map (\i -> (i ^. figi, i ^. interval)) $ r ^. candlesSubscriptions }
  Just (MarketDataResponse'SubscribeOrderBookResponse r) ->
    OrderBook { orderInsts = map (\i -> (i ^. figi, i ^. depth)) $ r ^. orderBookSubscriptions }
  Just (MarketDataResponse'SubscribeTradesResponse r) -> Trades { figis = map (^. figi) $ r ^. tradeSubscriptions }
  Just (MarketDataResponse'SubscribeInfoResponse r) -> Info { figis = map (^. figi) $ r ^. infoSubscriptions }
  Just (MarketDataResponse'SubscribeLastPriceResponse r) -> LastPrice { figis = map (^. figi) $ r ^. lastPriceSubscriptions }
  Just (MarketDataResponse'Candle r) -> Candles { candlesInsts = [(r ^. figi, r ^. interval)] }
  Just (MarketDataResponse'Trade r) -> Trades { figis = [r ^. figi] }
  Just (MarketDataResponse'Orderbook r) -> OrderBook { orderInsts = [(r ^. figi, r ^. depth)] }
  Just (MarketDataResponse'TradingStatus r) -> Info { figis = [r ^. figi] }
  Just (MarketDataResponse'LastPrice r) -> LastPrice { figis = [r ^. figi] }
  Just (MarketDataResponse'Ping r) -> Unknown
  Nothing -> Unknown

producedBy :: MarketDataResponse -> MarketDataRequest -> Bool
producedBy resp req = responseId resp == requestId req
