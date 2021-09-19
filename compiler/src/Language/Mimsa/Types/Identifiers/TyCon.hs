{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Identifiers.TyCon where

import qualified Data.Aeson as JSON
import qualified Data.Char as Ch
import Data.OpenApi
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Types.Identifiers.Name

-- | A TyCon is a Type Constructor like `Just` or `Right`.
-- It must start with a capital letter.
newtype TyCon = TyCon Text
  deriving newtype (ToSchema)
  deriving stock (Eq, Ord, Generic)
  deriving newtype
    ( Show,
      JSON.FromJSONKey,
      JSON.ToJSON,
      JSON.ToJSONKey
    )

instance JSON.FromJSON TyCon where
  parseJSON json =
    JSON.parseJSON json >>= \txt -> case safeMkTyCon txt of
      Just tyCon' -> pure tyCon'
      _ -> fail "Text is not a valid TyCon"

instance IsString TyCon where
  fromString = mkTyCon . T.pack

getTyCon :: TyCon -> Text
getTyCon (TyCon t) = t

validTyCon :: Text -> Bool
validTyCon a =
  T.length a > 0
    && T.filter Ch.isAlphaNum a == a
    && not (Ch.isDigit (T.head a))
    && Ch.isUpper (T.head a)

mkTyCon :: Text -> TyCon
mkTyCon a =
  if validTyCon a
    then TyCon a
    else error $ T.unpack $ "TyCon validation fail for '" <> a <> "'"

safeMkTyCon :: Text -> Maybe TyCon
safeMkTyCon a =
  if validTyCon a
    then Just (TyCon a)
    else Nothing

instance Printer TyCon where
  prettyDoc = pretty . getTyCon

tyConToName :: TyCon -> Name
tyConToName (TyCon tc) = mkName (tHead <> T.tail tc)
  where
    tHead = T.pack . pure . Ch.toLower . T.head $ tc
