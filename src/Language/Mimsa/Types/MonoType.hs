{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.MonoType
  ( MonoType (..),
  )
where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text.Prettyprint.Doc
import Language.Mimsa.Printer
import Language.Mimsa.Types.Identifiers

data MonoType
  = MTInt
  | MTString
  | MTBool
  | MTUnit
  | MTFunction MonoType MonoType -- argument, result
  | MTPair MonoType MonoType -- (a,b)
  | MTRecord (Map Name MonoType) -- { foo: a, bar: b }
  | MTVar Variable
  | MTData TyCon [MonoType] -- name, typeVars
  deriving (Eq, Ord, Show)

instance Printer MonoType where
  prettyDoc = renderMonoType

renderMonoType :: MonoType -> Doc ann
renderMonoType MTUnit = "Unit"
renderMonoType MTInt = "Int"
renderMonoType MTString = "String"
renderMonoType MTBool = "Boolean"
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
