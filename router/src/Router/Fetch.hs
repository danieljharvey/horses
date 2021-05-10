{-# LANGUAGE OverloadedStrings #-}

module Router.Fetch where

import Control.Monad.IO.Class
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Req

fetch :: (MonadIO m) => Url scheme -> Int -> JSON.Value -> m LBS.ByteString
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
