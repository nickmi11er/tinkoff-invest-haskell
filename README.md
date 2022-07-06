![Haskell](https://img.shields.io/badge/Haskell-5e5086?&logo=haskell&logoColor=white)
![LISENCE](https://img.shields.io/badge/LICENSE-MIT-green)

# tinkoff-invest-haskell

Haskell gRPC based SDK for [Tinkoff Invest API V2](https://github.com/Tinkoff/investAPI)

## Preparations

### Project dependency

The library is distributed via Hackage module. To use this SDK in your project you should add folowing line to dependencies block of `package.yaml`

```yaml
dependencies:
      - tinkoff-invest-sdk
```

### API Token

Tinkoff Invest API requires access token for comunications. [There is instructions](https://tinkoff.github.io/investAPI/token/) of generating personal token.

## Examples

### Unary queries

```haskell
getBaseShares :: GrpcClient -> IO [I.Share]
getBaseShares gc = (^. I.instruments) <$> (liftIO . runGrpc $ I.shares gc req)
  where req = defMessage & I.instrumentStatus .~ I.INSTRUMENT_STATUS_BASE

main :: IO ()
main = do
  let config = ClientConfig {
    token = "your_token",
    appName = Just "YourAppName"
  }
  client <- initGrpcClient config
  shares <- getBaseShares client
  mapM_ print shares
```

## Contribution

TODO