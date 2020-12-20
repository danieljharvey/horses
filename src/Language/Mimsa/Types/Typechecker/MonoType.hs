{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Typechecker.MonoType
  ( MonoType,
    Type (..),
    Primitive (..),
    getAnnotationForType,
  )
where

import qualified Data.Aeson as JSON
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text.Prettyprint.Doc
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers

data Primitive
  = MTInt
  | MTString
  | MTBool
  | MTUnit
  deriving (Eq, Ord, Show, Generic, JSON.ToJSON)

instance Printer Primitive where
  prettyDoc MTUnit = "Unit"
  prettyDoc MTInt = "Int"
  prettyDoc MTString = "String"
  prettyDoc MTBool = "Boolean"

type MonoType = Type Annotation

data Type ann
  = MTPrim ann Primitive
  | MTVar ann TypeIdentifier
  | MTFunction ann (Type ann) (Type ann) -- argument, result
  | MTPair ann (Type ann) (Type ann) -- (a,b)
  | MTRecord ann (Map Name (Type ann)) -- { foo: a, bar: b }
  | MTData ann TyCon [Type ann] -- name, typeVars
  deriving (Eq, Ord, Show, Functor, Generic, JSON.ToJSON)

getAnnotationForType :: Type ann -> ann
getAnnotationForType (MTPrim ann _) = ann
getAnnotationForType (MTVar ann _) = ann
getAnnotationForType (MTFunction ann _ _) = ann
getAnnotationForType (MTPair ann _ _) = ann
getAnnotationForType (MTRecord ann _) = ann
getAnnotationForType (MTData ann _ _) = ann

instance Printer (Type ann) where
  prettyDoc = renderMonoType

renderMonoType :: Type ann -> Doc a
renderMonoType (MTPrim _ a) = prettyDoc a
renderMonoType (MTFunction _ a b) =
  parens (renderMonoType a <+> "->" <+> renderMonoType b)
renderMonoType (MTPair _ a b) =
  tupled [renderMonoType a, renderMonoType b]
renderMonoType (MTRecord _ as) =
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
renderMonoType (MTVar _ a) = renderTypeIdentifier a
renderMonoType (MTData _ (TyCon n) vars) =
  align $ sep ([pretty n] <> (renderMonoType <$> vars))
