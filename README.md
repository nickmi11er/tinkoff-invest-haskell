![Haskell](https://img.shields.io/badge/Haskell-5e5086?&logo=haskell&logoColor=white)
![LISENCE](https://img.shields.io/badge/LICENSE-MIT-green)

# tinkoff-invest-haskell

Haskell gRPC based SDK for [Tinkoff Invest API V2](https://github.com/Tinkoff/investAPI)

## Preparations

### Project dependency

The library is distributed via Hackage [module](https://hackage.haskell.org/package/tinkoff-invest-sdk). To use this SDK in your project you should add folowing line to dependencies block of `package.yaml`

```yaml
dependencies:
      - tinkoff-invest-sdk
```

### API Token

Tinkoff Invest API requires access token for comunications. [There is instructions](https://tinkoff.github.io/investAPI/token/) of generating personal token.

## Examples

### Unary queries

```haskell
getBaseShares :: GrpcClient -> GrpcIO [Share]
getBaseShares gc = shares gc (defMessage & I.instrumentStatus .~ INSTRUMENT_STATUS_BASE)

main :: IO ()
main = void . runExceptT $ initClient <#> getBaseShares #> (liftIO . print)
    where initClient = initGrpcClient $ ClientConfig {
        token = "your_token",
        appName = Just "your_app_name"
    }
```

### Stream queries

```haskell
main :: IO ()
main = do
    let config = ClientConfig { token = "your_token", appName = Just "your_app_name" }
    client <- runClient config
    stream <- marketDataStream client

    subscribeOrderBook stream "BBG004730RP0" 10 \resp -> case resp ^. MD.maybe'payload of
        Just (MarketDataResponse'SubscribeOrderBookResponse r) -> logResponse r >> pure Next
        Just (MarketDataResponse'Orderbook r) -> logResponse r >> pure Next
        _ -> pure Break
        
    wait stream
```

_See more examples [here](/example/src)_

## TODO

- [ ] Cyrillic encoding support
- [ ] More enhanced calculation monad
- [ ] Trading stratagies

## Contribution

### Build project steps

1. Install [Stack](https://docs.haskellstack.org/en/stable/README/) tool
2. Install [ProtoBuff Compiller](https://grpc.io/docs/protoc-installation/)
3. Run `stack build` from project root directory