{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Core.Types.AST.DataType
  ( DataType (..),
  )
where

import qualified Data.Aeson as JSON
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import GHC.Generics (Generic)
import Language.Mimsa.Core.Printer (Printer (prettyDoc))
import Language.Mimsa.Core.Types.Identifiers
  ( Name,
    TyCon,
    renderName,
  )
import Language.Mimsa.Core.Types.Identifiers.TypeName
import Language.Mimsa.Core.Types.Type.MonoType
import Prettyprinter

-------

-- | This describes a custom data type, such as `Either e a = Left e | Right a`
data DataType = DataType
  { -- | The name of this type, ie `Either`
    dtName :: TypeName,
    -- | The type variables for the data type, ie `e`, `a`
    dtVars :: [Name],
    -- | map from constructor name to it's arguments, ie "`Left` -> [`e`]" or "`Right` -> [`a`]"
    dtConstructors :: Map TyCon [Type ()]
  }
  deriving stock
    ( Eq,
      Ord,
      Show,
      Generic
    )
  deriving anyclass
    ( JSON.FromJSON,
      JSON.ToJSON
    )

instance Printer DataType where
  prettyDoc = renderDataType

renderDataType :: DataType -> Doc style
renderDataType (DataType tyCon vars' constructors') =
  "type"
    <+> prettyDoc tyCon
    <> printVars vars'
    <+> if M.null constructors'
      then mempty
      else
        group $
          line
            <> indent
              2
              ( align $
                  vsep $
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
              vsep (prettyMt <$> args)
          )
    prettyMt mt = case mt of
      mtApp@MTTypeApp {} -> "(" <> prettyDoc mtApp <> ")"
      mtFunc@MTFunction {} -> "(" <> prettyDoc mtFunc <> ")"
      other -> prettyDoc other
