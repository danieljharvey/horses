{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server.Prometheus where

import Control.Exception
import Control.Monad
import qualified Data.HashMap.Strict as H
import Data.Kind
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock
import GHC.TypeLits
import Network.HTTP.Types (Method, Status (..), status200)
import Network.Wai
import Prometheus as Prom
import Servant.API as Servant

gaugeInflight :: Gauge -> Middleware
gaugeInflight inflight application request respond =
  bracket_
    (incGauge inflight)
    (decGauge inflight)
    (application request respond)

-- | Count responses with 2XX, 4XX, 5XX, and XXX response codes.
countResponseCodes ::
  Vector Label1 Counter ->
  Middleware
countResponseCodes codes application request respond =
  application request respond'
  where
    respond' res = count (responseStatus res) >> respond res
    count Status {statusCode = sc}
      | 200 <= sc && sc < 300 = withLabel codes "2XX" incCounter
      | 400 <= sc && sc < 500 = withLabel codes "4XX" incCounter
      | 500 <= sc && sc < 600 = withLabel codes "5XX" incCounter
      | otherwise = withLabel codes "XXX" incCounter

responseTimeDistribution :: MeasureQuantiles -> Histogram -> Maybe Prom.Summary -> Middleware
responseTimeDistribution _qants _hist Nothing application request respond = application request respond
responseTimeDistribution qants hist (Just qant) application request respond =
  bracket getCurrentTime stop $ const $ application request respond
  where
    stop t1 = do
      t2 <- getCurrentTime
      let dt = diffUTCTime t2 t1
          t = fromRational $ (* 1000) $ toRational dt
      observe hist t
      case qants of
        WithQuantiles -> observe qant t
        NoQuantiles -> pure ()

data Meters = Meters
  { metersInflight :: Gauge,
    metersResponses :: Vector Label1 Counter,
    metersTime :: Histogram,
    metersTimeQant :: Maybe Prom.Summary,
    metersRecordQuants :: MeasureQuantiles
  }

-- | Measuring quantiles can add significant overhead to your application if your
-- requests are often small. You should benchmark your app with and without
-- quantiles to decide if the overhead is acceptable for you application.
data MeasureQuantiles = WithQuantiles | NoQuantiles deriving stock (Show, Eq)

makeMeters :: HasEndpoints api => Proxy api -> MeasureQuantiles -> IO (H.HashMap Text Meters)
makeMeters proxy metersRecordQuants = do
  let eps =
        "unknown" :
        map
          (\(ps, method) -> T.intercalate "." $ ps <> [T.decodeUtf8 method])
          (getEndpoints proxy)
  ms <- forM eps $ \path -> do
    let prefix = "servant.path." <> path <> "."
        info :: Text -> Text -> Text -> Info
        info prfx name help = Info (prfx <> name) (help <> prfx)
    let mMetersInflight = gauge $ info prefix "in_flight" "Number of in flight requests for "
        mMetersResponses = vector "status_code" $ counter (info prefix "http_status" "Counters for status codes")
        mMetersTime =
          histogram
            (info prefix "time_ms" "Distribution of query times for ")
            [10, 50, 100, 150, 200, 300, 500, 1000, 1500, 2500, 5000, 7000, 10000, 50000]
        mMetersTimeQant = summary (info prefix "time_ms" "Summary of query times for ") defaultQuantiles

    metersInflight <- register mMetersInflight
    metersResponses <- register mMetersResponses
    metersTime <- register mMetersTime
    metersTimeQant <- case metersRecordQuants of
      NoQuantiles -> pure Nothing
      WithQuantiles -> Just <$> register mMetersTimeQant
    pure (path, Meters {..})
  pure $ H.fromList ms

monitorServant ::
  HasEndpoints api =>
  Proxy api ->
  H.HashMap Text Meters ->
  Middleware
monitorServant proxy ms application request respond = do
  let path = case getEndpoint proxy request of
        Nothing -> "unknown"
        Just (ps, method) -> T.intercalate "." $ ps <> [T.decodeUtf8 method]
  let Meters {..} = ms H.! path
      application' =
        responseTimeDistribution metersRecordQuants metersTime metersTimeQant
          . countResponseCodes metersResponses
          . gaugeInflight metersInflight
          $ application
  application' request respond

-- | An application which will always return prometheus metrics with status 200.
-- This can be added to a Servant API using the RAW type, or may be run in a
-- second webserver on a different port to keep metrics reporting separate from
-- your application.
servePrometheusMetrics :: Application
servePrometheusMetrics _req respond =
  respond . responseLBS status200 [] =<< exportMetricsAsText

class HasEndpoints a where
  getEndpoints :: Proxy a -> [([Text], Method)]
  getEndpoint :: Proxy a -> Request -> Maybe ([Text], Method)

instance (HasEndpoints (a :: Type), HasEndpoints (b :: Type)) => HasEndpoints (a :<|> b) where
  getEndpoints _ =
    getEndpoints (Proxy :: Proxy a) ++ getEndpoints (Proxy :: Proxy b)
  getEndpoint _ req =
    getEndpoint (Proxy :: Proxy a) req
      `mplus` getEndpoint (Proxy :: Proxy b) req

instance
  (KnownSymbol (path :: Symbol), HasEndpoints (sub :: Type)) =>
  HasEndpoints (path :> sub)
  where
  getEndpoints _ = do
    (end, method) <- getEndpoints (Proxy :: Proxy sub)
    return (T.pack (symbolVal (Proxy :: Proxy path)) : end, method)
  getEndpoint _ req =
    case pathInfo req of
      p : ps | p == T.pack (symbolVal (Proxy :: Proxy path)) -> do
        (end, method) <- getEndpoint (Proxy :: Proxy sub) req {pathInfo = ps}
        return (p : end, method)
      _ -> Nothing

#if MIN_VERSION_servant(0,13,0)
#define CAPTURE Capture' mods
#define HEADER Header' mods
#define QUERY_PARAM QueryParam' mods
#define REQ_BODY ReqBody' mods
#else
#define CAPTURE Capture
#define HEADER Header
#define QUERY_PARAM QueryParam
#define REQ_BODY ReqBody
#endif

instance
  (KnownSymbol (capture :: Symbol), HasEndpoints (sub :: Type)) =>
  HasEndpoints (CAPTURE capture a :> sub)
  where
  getEndpoints _ = do
    (end, method) <- getEndpoints (Proxy :: Proxy sub)
    let p = T.pack $ (':' :) $ symbolVal (Proxy :: Proxy capture)
    return (p : end, method)
  getEndpoint _ req =
    case pathInfo req of
      _ : ps -> do
        (end, method) <- getEndpoint (Proxy :: Proxy sub) req {pathInfo = ps}
        let p = T.pack $ (':' :) $ symbolVal (Proxy :: Proxy capture)
        return (p : end, method)
      _ -> Nothing

instance HasEndpoints (sub :: Type) => HasEndpoints (AuthProtect t :> sub) where
  getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: Type) => HasEndpoints (BasicAuth r a :> sub) where
  getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: Type) => HasEndpoints (HEADER h a :> sub) where
  getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: Type) => HasEndpoints (QUERY_PARAM (h :: Symbol) a :> sub) where
  getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: Type) => HasEndpoints (QueryParams (h :: Symbol) a :> sub) where
  getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: Type) => HasEndpoints (QueryFlag h :> sub) where
  getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: Type) => HasEndpoints (REQ_BODY cts a :> sub) where
  getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: Type) => HasEndpoints (RemoteHost :> sub) where
  getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: Type) => HasEndpoints (IsSecure :> sub) where
  getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: Type) => HasEndpoints (HttpVersion :> sub) where
  getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: Type) => HasEndpoints (Vault :> sub) where
  getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: Type) => HasEndpoints (WithNamedContext x y sub) where
  getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance ReflectMethod method => HasEndpoints (UVerb method contentTypes as) where
  getEndpoints _ = [([], method)]
    where
      method = reflectMethod (Proxy :: Proxy method)
  getEndpoint _ req = case pathInfo req of
    [] | requestMethod req == method -> Just ([], method)
    _ -> Nothing
    where
      method = reflectMethod (Proxy :: Proxy method)

instance ReflectMethod method => HasEndpoints (Verb method status cts a) where
  getEndpoints _ = [([], method)]
    where
      method = reflectMethod (Proxy :: Proxy method)
  getEndpoint _ req = case pathInfo req of
    [] | requestMethod req == method -> Just ([], method)
    _ -> Nothing
    where
      method = reflectMethod (Proxy :: Proxy method)

instance HasEndpoints Raw where
  getEndpoints _ = pure ([], "RAW")
  getEndpoint _ _ = Just ([], "RAW")

instance HasEndpoints EmptyAPI where
  getEndpoints _ = pure ([], "EmptyAPI")
  getEndpoint _ _ = Just ([], "EmptyAPI")

instance HasEndpoints (sub :: Type) => HasEndpoints (Servant.Description s :> sub) where
  getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

instance HasEndpoints (sub :: Type) => HasEndpoints (Servant.Summary s :> sub) where
  getEndpoints _ = getEndpoints (Proxy :: Proxy sub)
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)
