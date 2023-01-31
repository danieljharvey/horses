{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Smol.Core.Types.DataType
  ( DataType (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict
import qualified Data.Map.Strict as M
import GHC.Generics (Generic)
import Prettyprinter
import Smol.Core.Printer
import Smol.Core.Types.Constructor
import Smol.Core.Types.Identifier
import Smol.Core.Types.Type
import Smol.Core.Types.TypeName

data DataType dep ann = DataType
  { dtName :: TypeName,
    dtVars :: [Identifier],
    dtConstructors :: Map Constructor [Type dep ann]
  }
  deriving stock (Functor, Generic)

deriving stock instance
  ( Eq ann,
    Eq (dep Identifier),
    Eq (dep TypeName)
  ) =>
  Eq (DataType dep ann)

deriving stock instance
  ( Ord ann,
    Ord (dep Identifier),
    Ord (dep TypeName)
  ) =>
  Ord (DataType dep ann)

instance
  ( Printer (dep Identifier),
    Printer (dep TypeName)
  ) =>
  Printer (DataType dep ann)
  where
  prettyDoc = renderDataType

renderDataType ::
  (Printer (dep Identifier), Printer (dep TypeName)) =>
  DataType dep ann ->
  Doc style
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
    printVars as = space <> sep (prettyDoc <$> as)
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
      mtApp@TApp {} -> "(" <> prettyDoc mtApp <> ")"
      mtFunc@TFunc {} -> "(" <> prettyDoc mtFunc <> ")"
      other -> prettyDoc other
