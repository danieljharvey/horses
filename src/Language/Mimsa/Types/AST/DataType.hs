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
import Data.OpenApi
import Data.Text.Prettyprint.Doc
import GHC.Generics (Generic)
import Language.Mimsa.Printer (Printer (prettyDoc))
import Language.Mimsa.Types.Identifiers
  ( Name,
    TyCon,
    TypeName,
    renderName,
  )

-------

data DataType
  = DataType
      { dtName :: TyCon,
        dtVars :: [Name],
        dtConstructors :: Map TyCon [TypeName]
      }
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, JSON.ToJSON, ToSchema)

instance Printer DataType where
  prettyDoc = renderDataType

renderDataType :: DataType -> Doc ann
renderDataType (DataType tyCon vars' constructors') =
  "type" <+> prettyDoc tyCon
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
