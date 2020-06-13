{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Name where

import qualified Data.Aeson as JSON
import qualified Data.Char as Ch
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics

newtype Name = Name {getName' :: Text}
  deriving stock (Eq, Ord, Generic)
  deriving newtype
    ( Show,
      JSON.FromJSON,
      JSON.FromJSONKey,
      JSON.ToJSON,
      JSON.ToJSONKey
    )

getName :: Name -> Text
getName (Name t) = t

validName :: Text -> Bool
validName a =
  T.length a > 0
    && T.filter (Ch.isAlphaNum) a == a
    && Ch.isDigit (T.head a) == False

mkName :: Text -> Name
mkName a =
  if validName a
    then Name a
    else error $ T.unpack $ "name fail for '" <> a <> "'"

safeMkName :: Text -> Maybe Name
safeMkName a =
  if validName a
    then Just (Name a)
    else Nothing
