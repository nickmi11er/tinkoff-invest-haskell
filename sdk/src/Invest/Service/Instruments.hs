module Invest.Service.Instruments(
    tradingSchedules,
    bondBy,
    bonds,
    getBondCoupons,
    currencyBy,
    currencies,
    etfBy,
    etfs,
    futureBy,
    futures,
    shareBy,
    shares,
    getAccruedInterests,
    getFuturesMargin,
    getInstrumentBy,
    getDividends,
    getAssetBy,
    getAssets,
    getFavorites,
    editFavorites,
    getCountries,
    findInstrument,
    getBrands,
    getBrandBy
) where

import           Control.Lens                    ((^.))
import           Data.ProtoLens.Message          (defMessage)
import           Invest.Client.Helpers           (GrpcIO, runUnary, runUnary_, GrpcClient)
import           Network.GRPC.HTTP2.ProtoLens    (RPC (..))
import           Proto.Invest.Instruments
import qualified Proto.Invest.Instruments_Fields as I

tradingSchedules :: GrpcClient -> TradingSchedulesRequest -> GrpcIO [TradingSchedule]
tradingSchedules = runUnary_ (^. I.exchanges) (RPC :: RPC InstrumentsService "tradingSchedules")

bondBy :: GrpcClient -> InstrumentRequest -> GrpcIO Bond
bondBy = runUnary_ (^. I.instrument) (RPC :: RPC InstrumentsService "bondBy")

bonds :: GrpcClient -> InstrumentsRequest -> GrpcIO [Bond]
bonds = runUnary_ (^. I.instruments) (RPC :: RPC InstrumentsService "bonds")

getBondCoupons :: GrpcClient -> GetBondCouponsRequest -> GrpcIO [Coupon]
getBondCoupons = runUnary_ (^. I.events) (RPC :: RPC InstrumentsService "getBondCoupons")

currencyBy :: GrpcClient -> InstrumentRequest -> GrpcIO Currency
currencyBy = runUnary_ (^. I.instrument) (RPC :: RPC InstrumentsService "currencyBy")

currencies :: GrpcClient -> InstrumentsRequest -> GrpcIO [Currency]
currencies = runUnary_ (^. I.instruments) (RPC :: RPC InstrumentsService "currencies")

etfBy :: GrpcClient -> InstrumentRequest -> GrpcIO Etf
etfBy = runUnary_ (^. I.instrument) (RPC :: RPC InstrumentsService "etfBy")

etfs :: GrpcClient -> InstrumentsRequest -> GrpcIO [Etf]
etfs = runUnary_ (^. I.instruments) (RPC :: RPC InstrumentsService "etfs")

futureBy :: GrpcClient -> InstrumentRequest -> GrpcIO Future
futureBy = runUnary_ (^. I.instrument) (RPC :: RPC InstrumentsService "futureBy")

futures :: GrpcClient -> InstrumentsRequest -> GrpcIO [Future]
futures = runUnary_ (^. I.instruments) (RPC :: RPC InstrumentsService "futures")

shareBy :: GrpcClient -> InstrumentRequest -> GrpcIO Share
shareBy = runUnary_ (^. I.instrument) (RPC :: RPC InstrumentsService "shareBy")

shares :: GrpcClient -> InstrumentsRequest -> GrpcIO [Share]
shares = runUnary_ (^. I.instruments) (RPC :: RPC InstrumentsService "shares")

getAccruedInterests :: GrpcClient -> GetAccruedInterestsRequest -> GrpcIO [AccruedInterest]
getAccruedInterests = runUnary_ (^. I.accruedInterests) (RPC :: RPC InstrumentsService "getAccruedInterests")

getFuturesMargin :: GrpcClient -> GetFuturesMarginRequest -> GrpcIO GetFuturesMarginResponse
getFuturesMargin = runUnary (RPC :: RPC InstrumentsService "getFuturesMargin")

getInstrumentBy :: GrpcClient -> InstrumentRequest -> GrpcIO Instrument
getInstrumentBy = runUnary_ (^. I.instrument) (RPC :: RPC InstrumentsService "getInstrumentBy")

getDividends :: GrpcClient -> GetDividendsRequest -> GrpcIO [Dividend]
getDividends = runUnary_ (^. I.dividends) (RPC :: RPC InstrumentsService "getDividends")

getAssetBy :: GrpcClient -> AssetRequest -> GrpcIO AssetFull
getAssetBy = runUnary_ (^. I.asset) (RPC :: RPC InstrumentsService "getAssetBy")

getAssets :: GrpcClient -> GrpcIO [Asset]
getAssets gc = runUnary_ (^. I.assets) (RPC :: RPC InstrumentsService "getAssets") gc defMessage

getFavorites :: GrpcClient -> GrpcIO[FavoriteInstrument]
getFavorites gc = runUnary_ (^. I.favoriteInstruments) (RPC :: RPC InstrumentsService "getFavorites") gc defMessage

editFavorites :: GrpcClient -> EditFavoritesRequest -> GrpcIO [FavoriteInstrument]
editFavorites = runUnary_ (^. I.favoriteInstruments) (RPC :: RPC InstrumentsService "editFavorites")

getCountries :: GrpcClient -> GrpcIO [CountryResponse]
getCountries gc = runUnary_ (^. I.countries) (RPC :: RPC InstrumentsService "getCountries") gc defMessage

findInstrument :: GrpcClient -> FindInstrumentRequest -> GrpcIO [InstrumentShort]
findInstrument = runUnary_ (^. I.instruments) (RPC :: RPC InstrumentsService "findInstrument")

getBrands :: GrpcClient -> GrpcIO [Brand]
getBrands gc = runUnary_ (^. I.brands) (RPC :: RPC InstrumentsService "getBrands") gc defMessage

getBrandBy :: GrpcClient -> GetBrandRequest -> GrpcIO Brand
getBrandBy = runUnary (RPC :: RPC InstrumentsService "getBrandBy")
