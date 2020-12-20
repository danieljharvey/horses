{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Identifiers.TypeIdentifier
  ( TypeIdentifier (..),
  )
where

-- the two types of id a type var can have - a named or numbered one

import qualified Data.Aeson as JSON
import Data.Text.Prettyprint.Doc
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Types.Identifiers.Name

data TypeIdentifier
  = TVName Name
  | TVNum Int
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON, JSON.ToJSONKey)

instance Printer TypeIdentifier where
  prettyDoc = renderVariable

printTypeNum :: Int -> String
printTypeNum i = [toEnum (index + start)] <> suffix
  where
    index = (i - 1) `mod` 26
    start = fromEnum 'A'
    suffix =
      let diff = (i - 1) `div` 26
       in if diff < 1 then "" else show diff

renderVariable :: TypeIdentifier -> Doc ann
renderVariable (TVName n) = renderName n
renderVariable (TVNum i) = pretty (printTypeNum i)
