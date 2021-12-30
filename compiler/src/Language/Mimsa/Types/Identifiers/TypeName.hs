{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Identifiers.TypeName (TypeName (..), typeNameToName) where

import qualified Data.Aeson as JSON
import qualified Data.Char as Ch
import Data.OpenApi
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Types.Identifiers.Name
import Prettyprinter

-- | A TypeName is `Maybe` or `Either`
-- It must start with a capital letter.
newtype TypeName = TypeName Text
  deriving newtype (ToSchema)
  deriving stock (Eq, Ord, Generic)
  deriving newtype
    ( Show,
      JSON.FromJSONKey,
      JSON.ToJSON,
      JSON.ToJSONKey
    )

instance JSON.FromJSON TypeName where
  parseJSON json =
    JSON.parseJSON json >>= \txt -> case safeMkTypeName txt of
      Just tyCon' -> pure tyCon'
      _ -> fail "Text is not a valid TypeName"

instance IsString TypeName where
  fromString = mkTypeName . T.pack

getTypeName :: TypeName -> Text
getTypeName (TypeName t) = t

validTypeName :: Text -> Bool
validTypeName a =
  T.length a > 0
    && T.filter Ch.isAlphaNum a == a
    && not (Ch.isDigit (T.head a))
    && Ch.isUpper (T.head a)

mkTypeName :: Text -> TypeName
mkTypeName a =
  if validTypeName a
    then TypeName a
    else error $ T.unpack $ "TypeName validation fail for '" <> a <> "'"

safeMkTypeName :: Text -> Maybe TypeName
safeMkTypeName a =
  if validTypeName a
    then Just (TypeName a)
    else Nothing

instance Printer TypeName where
  prettyDoc = pretty . getTypeName

typeNameToName :: TypeName -> Name
typeNameToName (TypeName tc) = mkName (tHead <> T.tail tc)
  where
    tHead = T.pack . pure . Ch.toLower . T.head $ tc
