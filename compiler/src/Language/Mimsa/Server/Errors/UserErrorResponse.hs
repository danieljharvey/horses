{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

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

data UserErrorResponse = UserErrorResponse
  { ueText :: Text,
    ueErrorSpans :: [Text]
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (JSON.ToJSON, ToSchema)

toUserError :: (Show ann, Printer ann) => Error ann -> UserErrorResponse
toUserError err =
  UserErrorResponse
    { ueText = prettyPrint err,
      ueErrorSpans = []
    }
