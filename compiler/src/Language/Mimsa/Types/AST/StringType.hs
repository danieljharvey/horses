{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Mimsa.Types.AST.StringType
  ( StringType (..),
    stringLength,
    stringSplit,
  )
where

import qualified Data.Aeson as JSON
import Data.String
import Data.Swagger
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import GHC.Generics
import Language.Mimsa.Printer

-- |
-- Type for our strings, that removes a number of characters that make
-- parsing complicated.
-- This should probably be revisited at some point
newtype StringType = StringType Text
  deriving newtype (Eq, Ord, Show, JSON.FromJSON, JSON.ToJSON, ToSchema)
  deriving stock (Generic)

instance IsString StringType where
  fromString = StringType . T.pack

instance Printer StringType where
  prettyDoc = renderStringType

renderStringType :: StringType -> Doc ann
renderStringType (StringType s) = pretty s

stringLength :: StringType -> Int
stringLength (StringType s) = T.length s

-- if there is string, return (head, tail)
stringSplit :: StringType -> Maybe (StringType, StringType)
stringSplit st@(StringType s) =
  if stringLength st == 0
    then Nothing
    else
      Just
        ( StringType (T.singleton (T.head s)),
          StringType (T.tail s)
        )
