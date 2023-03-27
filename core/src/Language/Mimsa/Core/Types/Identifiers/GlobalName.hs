{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Core.Types.Identifiers.GlobalName (GlobalName(..)) where

import qualified Data.Aeson as JSON
import qualified Data.Char as Ch
import Data.OpenApi
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Language.Mimsa.Core.Printer
import Prettyprinter
import Servant (FromHttpApiData)

renderGlobalName :: GlobalName -> Doc ann
renderGlobalName = pretty . getGlobalName

-- | A name is an identifier that starts with a lowercase
-- letter used for functions and values. Examples are
-- `dog`, `cat` but not `Bat`.
newtype GlobalName = GlobalName {getGlobalName' :: Text}
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

instance JSON.FromJSON GlobalName where
  parseJSON json =
    JSON.parseJSON json >>= \txt -> case safeMkGlobalName txt of
      Just name' -> pure name'
      _ -> fail "Text is not a valid name"

instance IsString GlobalName where
  fromString = mkGlobalName . T.pack

getGlobalName :: GlobalName -> Text
getGlobalName (GlobalName t) = t

validGlobalName :: Text -> Bool
validGlobalName a =
  T.length a > 0
    && T.filter Ch.isAlphaNum a == a
    && not (Ch.isDigit (T.head a))
    && Ch.isLower (T.head a)

mkGlobalName :: Text -> GlobalName
mkGlobalName a =
  if validGlobalName a
    then GlobalName a
    else error $ T.unpack $ "GlobalName validation fail for '" <> a <> "'"

safeMkGlobalName :: Text -> Maybe GlobalName
safeMkGlobalName a =
  if validGlobalName a
    then Just (GlobalName a)
    else Nothing

instance Printer GlobalName where
  prettyDoc = renderGlobalName
