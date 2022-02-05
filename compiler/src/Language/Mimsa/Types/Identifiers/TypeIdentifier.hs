{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Identifiers.TypeIdentifier
  ( TypeIdentifier (..),
    renderTypeIdentifier,
    printTypeNum,
    getUniVar,
  )
where

-- the two types of id a type var can have - a named or numbered one

import qualified Data.Aeson as JSON
import Data.Maybe
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Types.Identifiers.TyVar
import Prettyprinter

data TypeIdentifier
  = TVName
      { tiNum :: Maybe Int,
        tiVar :: TyVar
      }
  | TVUnificationVar
      { tiUniVar :: Int
      }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Generic
    )
  deriving anyclass
    ( JSON.ToJSON,
      JSON.ToJSONKey,
      JSON.FromJSON
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
renderTypeIdentifier (TVName _ n) = renderTyVar n
renderTypeIdentifier (TVUnificationVar i) = pretty (printTypeNum i)

getUniVar :: TypeIdentifier -> Int
getUniVar (TVName i _) = fromMaybe 0 i
getUniVar (TVUnificationVar i) = i
