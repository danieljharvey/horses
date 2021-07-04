{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Router.Fetch where

import Control.Monad.Except
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..))
import Network.HTTP.Req

data HTTPError
  = FourXX LBS.ByteString
  | OtherError

newtype FetchM a = FetchM {getFetchM :: ExceptT HTTPError IO a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadError HTTPError
    )

instance MonadHttp FetchM where
  handleHttpException (VanillaHttpException (HttpExceptionRequest _ (StatusCodeException _resp msg))) = throwError (FourXX (LBS.fromStrict msg))
  handleHttpException _ = throwError OtherError

fetch :: Url scheme -> Int -> JSON.Value -> FetchM LBS.ByteString
fetch url apiPort postData = runReq defaultHttpConfig $ do
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

fetchIO :: (MonadIO m) => Url scheme -> Int -> JSON.Value -> m (Either HTTPError LBS.ByteString)
fetchIO url apiPort postData = liftIO $ runExceptT (getFetchM comp)
  where
    comp = fetch url apiPort postData
