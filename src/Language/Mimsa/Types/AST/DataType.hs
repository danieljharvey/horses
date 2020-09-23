{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.AST.DataType
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
  "type" <+> prettyDoc construct'
    <> printVars vars'
    <+> if M.null constructors'
      then mempty
      else
        line
          <> indent
            2
            ( align $ vsep $
                zipWith
                  (<+>)
                  ("=" : repeat "|")
                  (printCons <$> M.toList constructors')
            )
  where
    printVars [] = mempty
    printVars as = space <> sep (renderName <$> as)
    printCons (consName, []) = prettyDoc consName
    printCons (consName, args) =
      prettyDoc consName
        <> softline
        <> hang
          0
          ( align $
              vsep (prettyDoc <$> args)
          )
