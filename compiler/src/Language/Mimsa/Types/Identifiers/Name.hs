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
import GHC.Generics
import Language.Mimsa.Printer
import Prettyprinter
import Servant

renderName :: Name -> Doc ann
renderName = pretty . getName

-- | A name is an identifier that starts with a lowercase
-- letter used for functions and values. Examples are
-- `dog`, `cat` but not `Bat`.
newtype Name = Name {getName' :: Text}
  deriving newtype (ToSchema, ToParamSchema)
  deriving stock (Eq, Ord, Generic)
  deriving newtype
    ( Show,
      JSON.FromJSONKey,
      JSON.ToJSONKey,
      JSON.ToJSON,
      FromHttpApiData,
      Semigroup,
      Monoid
    )

instance JSON.FromJSON Name where
  parseJSON json =
    JSON.parseJSON json >>= \txt -> case safeMkName txt of
      Just name' -> pure name'
      _ -> fail "Text is not a valid name"

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
