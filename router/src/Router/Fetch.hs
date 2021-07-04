{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Router.Fetch where

import Control.Monad.Except
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Req

newtype HTTPError
  = FourXX HttpException

newtype FetchM a = FetchM {getFetchM :: ExceptT HTTPError IO a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadError HTTPError
    )

instance MonadHttp FetchM where
  handleHttpException a = throwError (FourXX a)

fetch :: Url scheme -> Int -> JSON.Value -> FetchM LBS.ByteString
fetch url apiPort postData = do
  r <-
    req
      POST -- method
      url
      (ReqBodyJson postData) -- use built-in options or add your own
      lbsResponse -- specify how to interpret response
      ( port
          apiPort
      )
  pure (responseBody r)

fetchIO ::
  (MonadIO m) =>
  Url scheme ->
  Int ->
  JSON.Value ->
  m (Either HTTPError LBS.ByteString)
fetchIO url apiPort postData = liftIO $ runExceptT (getFetchM comp)
  where
    comp = fetch url apiPort postData
