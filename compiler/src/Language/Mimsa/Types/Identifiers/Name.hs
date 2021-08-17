{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Identifiers.Name where

import qualified Data.Aeson as JSON
import qualified Data.Char as Ch
import Data.OpenApi
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import GHC.Generics
import Language.Mimsa.Printer
import Servant

renderName :: Name -> Doc ann
renderName = pretty . getName

newtype Name = Name {getName' :: Text}
  deriving newtype (ToSchema, ToParamSchema)
  deriving stock (Eq, Ord, Generic)
  deriving newtype
    ( Show,
      JSON.FromJSON,
      JSON.FromJSONKey,
      JSON.ToJSON,
      JSON.ToJSONKey,
      FromHttpApiData,
      Semigroup,
      Monoid
    )

instance IsString Name where
  fromString = mkName . T.pack

getName :: Name -> Text
getName (Name t) = t

validName :: Text -> Bool
validName a =
  T.length a > 0
    && T.filter Ch.isAlphaNum a == a
    && not (Ch.isDigit (T.head a))
    && Ch.isLower (T.head a)

mkName :: Text -> Name
mkName a =
  if validName a
    then Name a
    else error $ T.unpack $ "Name validation fail for '" <> a <> "'"

safeMkName :: Text -> Maybe Name
safeMkName a =
  if validName a
    then Just (Name a)
    else Nothing

instance Printer Name where
  prettyDoc = renderName
