{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.MonoType
  ( MonoType (..),
    Primitive (..),
  )
where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text.Prettyprint.Doc
import Language.Mimsa.Printer
import Language.Mimsa.Types.Identifiers

data Primitive
  = MTInt
  | MTString
  | MTBool
  | MTUnit
  deriving (Eq, Ord, Show)

instance Printer Primitive where
  prettyDoc MTUnit = "Unit"
  prettyDoc MTInt = "Int"
  prettyDoc MTString = "String"
  prettyDoc MTBool = "Boolean"

data MonoType
  = MTPrim Primitive
  | MTVar Variable
  | MTFunction MonoType MonoType -- argument, result
  | MTPair MonoType MonoType -- (a,b)
  | MTRecord (Map Name MonoType) -- { foo: a, bar: b }
  | MTData TyCon [MonoType] -- name, typeVars
  deriving (Eq, Ord, Show)

instance Printer MonoType where
  prettyDoc = renderMonoType

renderMonoType :: MonoType -> Doc ann
renderMonoType (MTPrim a) = prettyDoc a
renderMonoType (MTFunction a b) =
  parens (renderMonoType a <+> "->" <+> renderMonoType b)
renderMonoType (MTPair a b) =
  tupled [renderMonoType a, renderMonoType b]
renderMonoType (MTRecord as) =
  enclose
    lbrace
    rbrace
    ( mconcat $
        punctuate
          comma
          ( indent 1 . renderItem
              <$> M.toList as
          )
    )
  where
    renderItem (Name k, v) = pretty k <+> ":" <+> renderMonoType v
renderMonoType (MTVar a) = case a of
  (NamedVar (Name n)) -> pretty n
  (NumberedVar i) -> pretty i
  (BuiltIn (Name n)) -> pretty n
  (BuiltInActual (Name n) _) -> pretty n
renderMonoType (MTData (TyCon n) vars) = align $ sep ([pretty n] <> (renderMonoType <$> vars))
