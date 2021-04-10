{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Identifiers.TypeIdentifier
  ( TypeIdentifier (..),
    renderTypeIdentifier,
  )
where

-- the two types of id a type var can have - a named or numbered one

import qualified Data.Aeson as JSON
import Data.Swagger
import Data.Text.Prettyprint.Doc
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Types.Identifiers.TyVar

data TypeIdentifier
  = TVName TyVar
  | TVNum Int
  deriving
    ( Eq,
      Ord,
      Show,
      Generic,
      JSON.ToJSON,
      JSON.ToJSONKey,
      JSON.FromJSON,
      ToSchema
    )

instance Printer TypeIdentifier where
  prettyDoc = renderTypeIdentifier

printTypeNum :: Int -> String
printTypeNum i = [toEnum (index + start)] <> suffix
  where
    index = (i - 1) `mod` 26
    start = fromEnum 'a'
    suffix =
      let diff = (i - 1) `div` 26
       in if diff < 1 then "" else show diff

renderTypeIdentifier :: TypeIdentifier -> Doc ann
renderTypeIdentifier (TVName n) = renderTyVar n
renderTypeIdentifier (TVNum i) = pretty (printTypeNum i)
