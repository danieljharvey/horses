{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Identifiers.TyVar
  ( TyVar (..),
    renderTyVar,
    mkTyVar,
    safeMkTyVar,
  )
where

-- a TyVar is the string token for any unknown type

import qualified Data.Aeson as JSON
import qualified Data.Char as Ch
import Data.String
import Data.Swagger
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import GHC.Generics
import Language.Mimsa.Printer

renderTyVar :: TyVar -> Doc ann
renderTyVar = pretty . getTyVar

newtype TyVar = TyVar {getTyVar' :: Text}
  deriving newtype (ToSchema)
  deriving stock (Eq, Ord, Generic)
  deriving newtype
    ( Show,
      JSON.FromJSON,
      JSON.FromJSONKey,
      JSON.ToJSON,
      JSON.ToJSONKey
    )

instance IsString TyVar where
  fromString = mkTyVar . T.pack

getTyVar :: TyVar -> Text
getTyVar (TyVar t) = t

-- a valid TyVar is non-empty, only consists of letters and numbers, and starts
-- with a letter
validTyVar :: Text -> Bool
validTyVar a =
  T.length a > 0
    && T.filter Ch.isAlphaNum a == a
    && not (Ch.isDigit (T.head a))
    && Ch.isLower (T.head a)

mkTyVar :: Text -> TyVar
mkTyVar a =
  if validTyVar a
    then TyVar a
    else error $ T.unpack $ "TyVar validation fail for '" <> a <> "'"

safeMkTyVar :: Text -> Maybe TyVar
safeMkTyVar a =
  if validTyVar a
    then Just (TyVar a)
    else Nothing

instance Printer TyVar where
  prettyDoc = renderTyVar
