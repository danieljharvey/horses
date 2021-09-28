{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Language.Mimsa.Server.Errors.UserErrorResponse
  ( UserErrorResponse (..),
    toUserError,
  )
where

import qualified Data.Aeson as JSON
import Data.OpenApi
import Data.Text (Text)
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Types.Error
import Servant

data UserErrorResponse = UserErrorResponse
  { ueText :: Text,
    ueErrorSpans :: [Text]
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

-- set return type for regular response
instance HasStatus UserErrorResponse where
  type StatusOf UserErrorResponse = 400

toUserError :: (Show ann, Printer ann) => Error ann -> UserErrorResponse
toUserError err =
  UserErrorResponse
    { ueText = prettyPrint err,
      ueErrorSpans = []
    }
