{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Types.TypeclassName
  ( TypeclassName (..),
    getTypeclassName,
    validTypeclassName,
    safeMkTypeclassName,
  )
where

import qualified Data.Aeson as JSON
import qualified Data.Char as Ch
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Prettyprinter
import Smol.Core.Printer

-- | A TypeclassName is like `Either` or `Maybe`.
-- It must start with a capital letter.
newtype TypeclassName = TypeclassName Text
  deriving stock (Eq, Ord, Generic)
  deriving newtype
    ( Show,
      JSON.FromJSONKey,
      JSON.ToJSON,
      JSON.ToJSONKey
    )

instance JSON.FromJSON TypeclassName where
  parseJSON json =
    JSON.parseJSON json >>= \txt -> case safeMkTypeclassName txt of
      Just tyCon' -> pure tyCon'
      _ -> fail "Text is not a valid TypeclassName"

instance IsString TypeclassName where
  fromString = mkTypeclassName . T.pack

getTypeclassName :: TypeclassName -> Text
getTypeclassName (TypeclassName t) = t

validTypeclassName :: Text -> Bool
validTypeclassName a =
  T.length a > 0
    && T.filter Ch.isAlphaNum a == a
    && not (Ch.isDigit (T.head a))
    && Ch.isUpper (T.head a)

mkTypeclassName :: Text -> TypeclassName
mkTypeclassName a =
  if validTypeclassName a
    then TypeclassName a
    else error $ T.unpack $ "TypeclassName validation fail for '" <> a <> "'"

safeMkTypeclassName :: Text -> Maybe TypeclassName
safeMkTypeclassName a =
  if validTypeclassName a
    then Just (TypeclassName a)
    else Nothing

instance Printer TypeclassName where
  prettyDoc = pretty . getTypeclassName
