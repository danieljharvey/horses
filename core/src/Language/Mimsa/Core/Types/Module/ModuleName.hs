{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Core.Types.Module.ModuleName
  ( ModuleName (..),
    getModuleName,
    validModuleName,
    safeMkModuleName,
  )
where

import Data.Hashable
import qualified Data.Aeson as JSON
import qualified Data.Char as Ch
import Data.OpenApi
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Language.Mimsa.Core.Printer
import Prettyprinter

-- | A ModuleName is like `Either` or `Maybe`.
-- It must start with a capital letter.
newtype ModuleName = ModuleName Text
  deriving newtype (ToSchema)
  deriving stock (Eq, Ord, Generic)
  deriving newtype
    ( Show,
      JSON.FromJSONKey,
      JSON.ToJSON,
      JSON.ToJSONKey,
      Hashable
    )

instance JSON.FromJSON ModuleName where
  parseJSON json =
    JSON.parseJSON json >>= \txt -> case safeMkModuleName txt of
      Just tyCon' -> pure tyCon'
      _ -> fail "Text is not a valid ModuleName"

instance IsString ModuleName where
  fromString = mkModuleName . T.pack

getModuleName :: ModuleName -> Text
getModuleName (ModuleName t) = t

validModuleName :: Text -> Bool
validModuleName a =
  T.length a > 0
    && T.filter Ch.isAlphaNum a == a
    && not (Ch.isDigit (T.head a))
    && Ch.isUpper (T.head a)

mkModuleName :: Text -> ModuleName
mkModuleName a =
  if validModuleName a
    then ModuleName a
    else error $ T.unpack $ "ModuleName validation fail for '" <> a <> "'"

safeMkModuleName :: Text -> Maybe ModuleName
safeMkModuleName a =
  if validModuleName a
    then Just (ModuleName a)
    else Nothing

instance Printer ModuleName where
  prettyDoc = pretty . getModuleName
