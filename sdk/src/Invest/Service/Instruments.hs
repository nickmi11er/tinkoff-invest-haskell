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

import           Data.ProtoLens.Message       (defMessage)
import           Network.GRPC.Client          (RawReply)
import           Network.GRPC.Client.Helpers  (GrpcClient, rawUnary)
import           Network.GRPC.HTTP2.ProtoLens (RPC (..))
import           Network.HTTP2.Client         (ClientIO, TooMuchConcurrency)
import           Proto.Invest.Instruments     as I

tradingSchedules
    :: GrpcClient
    -> I.TradingSchedulesRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply I.TradingSchedulesResponse))
tradingSchedules = rawUnary (RPC :: RPC I.InstrumentsService "tradingSchedules")

bondBy
    :: GrpcClient
    -> I.InstrumentRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply I.BondResponse))
bondBy = rawUnary (RPC :: RPC I.InstrumentsService "bondBy")

bonds
    :: GrpcClient
    -> I.InstrumentsRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply I.BondsResponse))
bonds = rawUnary (RPC :: RPC I.InstrumentsService "bonds")

getBondCoupons
    :: GrpcClient
    -> I.GetBondCouponsRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply I.GetBondCouponsResponse))
getBondCoupons = rawUnary (RPC :: RPC I.InstrumentsService "getBondCoupons")

currencyBy
    :: GrpcClient
    -> I.InstrumentRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply I.CurrencyResponse))
currencyBy = rawUnary (RPC :: RPC I.InstrumentsService "currencyBy")

currencies
    :: GrpcClient
    -> I.InstrumentsRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply I.CurrenciesResponse))
currencies = rawUnary (RPC :: RPC I.InstrumentsService "currencies")

etfBy
    :: GrpcClient
    -> I.InstrumentRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply I.EtfResponse))
etfBy = rawUnary (RPC :: RPC I.InstrumentsService "etfBy")

etfs
    :: GrpcClient
    -> I.InstrumentsRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply I.EtfsResponse))
etfs = rawUnary (RPC :: RPC I.InstrumentsService "etfs")

futureBy
    :: GrpcClient
    -> I.InstrumentRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply I.FutureResponse))
futureBy = rawUnary (RPC :: RPC I.InstrumentsService "futureBy")

futures
    :: GrpcClient
    -> I.InstrumentsRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply I.FuturesResponse))
futures = rawUnary (RPC :: RPC I.InstrumentsService "futures")

shareBy
    :: GrpcClient
    -> I.InstrumentRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply I.ShareResponse))
shareBy = rawUnary (RPC :: RPC I.InstrumentsService "shareBy")

shares
    :: GrpcClient
    -> I.InstrumentsRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply I.SharesResponse))
shares = rawUnary (RPC :: RPC I.InstrumentsService "shares")

getAccruedInterests
    :: GrpcClient
    -> I.GetAccruedInterestsRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply I.GetAccruedInterestsResponse))
getAccruedInterests = rawUnary (RPC :: RPC I.InstrumentsService "getAccruedInterests")

getFuturesMargin
    :: GrpcClient
    -> I.GetFuturesMarginRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply I.GetFuturesMarginResponse))
getFuturesMargin = rawUnary (RPC :: RPC I.InstrumentsService "getFuturesMargin")

getInstrumentBy
    :: GrpcClient
    -> I.InstrumentRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply I.InstrumentResponse))
getInstrumentBy = rawUnary (RPC :: RPC I.InstrumentsService "getInstrumentBy")

getDividends
    :: GrpcClient
    -> I.GetDividendsRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply I.GetDividendsResponse))
getDividends = rawUnary (RPC :: RPC I.InstrumentsService "getDividends")

getAssetBy
    :: GrpcClient
    -> I.AssetRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply I.AssetResponse))
getAssetBy = rawUnary (RPC :: RPC I.InstrumentsService "getAssetBy")

getAssets
    :: GrpcClient
    -> ClientIO (Either TooMuchConcurrency (RawReply I.AssetsResponse))
getAssets gc = rawUnary (RPC :: RPC I.InstrumentsService "getAssets") gc defMessage

getFavorites
    :: GrpcClient
    -> ClientIO (Either TooMuchConcurrency (RawReply I.GetFavoritesResponse))
getFavorites gc = rawUnary (RPC :: RPC I.InstrumentsService "getFavorites") gc defMessage

editFavorites
    :: GrpcClient
    -> I.EditFavoritesRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply I.EditFavoritesResponse))
editFavorites = rawUnary (RPC :: RPC I.InstrumentsService "editFavorites")

getCountries
    :: GrpcClient
    -> ClientIO (Either TooMuchConcurrency (RawReply I.GetCountriesResponse))
getCountries gc = rawUnary (RPC :: RPC I.InstrumentsService "getCountries") gc defMessage

findInstrument
    :: GrpcClient
    -> I.FindInstrumentRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply I.FindInstrumentResponse))
findInstrument = rawUnary (RPC :: RPC I.InstrumentsService "findInstrument")

getBrands
    :: GrpcClient
    -> ClientIO (Either TooMuchConcurrency (RawReply I.GetBrandsResponse))
getBrands gc = rawUnary (RPC :: RPC I.InstrumentsService "getBrands") gc defMessage

getBrandBy
    :: GrpcClient
    -> I.GetBrandRequest
    -> ClientIO (Either TooMuchConcurrency (RawReply I.Brand))
getBrandBy = rawUnary (RPC :: RPC I.InstrumentsService "getBrandBy")
