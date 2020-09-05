{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Construct where

import qualified Data.Aeson as JSON
import qualified Data.Char as Ch
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics

newtype Construct = Construct {getConstruct' :: Text}
  deriving stock (Eq, Ord, Generic)
  deriving newtype
    ( Show,
      JSON.FromJSON,
      JSON.FromJSONKey,
      JSON.ToJSON,
      JSON.ToJSONKey
    )

getConstruct :: Construct -> Text
getConstruct (Construct t) = t

validConstruct :: Text -> Bool
validConstruct a =
  T.length a > 0
    && T.filter Ch.isAlphaNum a == a
    && not (Ch.isDigit (T.head a))
    && Ch.isUpper (T.head a)

mkConstruct :: Text -> Construct
mkConstruct a =
  if validConstruct a
    then Construct a
    else error $ T.unpack $ "Construct validation fail for '" <> a <> "'"

safeMkConstruct :: Text -> Maybe Construct
safeMkConstruct a =
  if validConstruct a
    then Just (Construct a)
    else Nothing
