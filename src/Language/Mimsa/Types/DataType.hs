{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.DataType
  ( DataType (..),
  )
where

import qualified Data.Aeson as JSON
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text.Prettyprint.Doc
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Types.Identifiers

-------

data DataType
  = DataType
      { dtName :: Construct,
        dtVars :: [Name],
        dtConstructors :: Map Construct [TypeName]
      }
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, JSON.ToJSON)

instance Printer DataType where
  prettyDoc = renderDataType

renderDataType :: DataType -> Doc ann
renderDataType (DataType construct' vars' constructors') =
  "type " <+> prettyDoc construct'
    <+> printVars vars'
    <+> if M.null constructors'
      then space
      else
        sep $
          zipWith
            (<+>)
            ("=" : repeat "|")
            (printCons <$> M.toList constructors')
  where
    printVars [] = mempty
    printVars as = sep $ renderName <$> as
    printCons (consName, args) =
      sep $ [prettyDoc consName] <> (prettyDoc <$> args)
