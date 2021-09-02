{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Mimsa.Types.Typechecker.MonoType
  ( MonoType,
    Type (..),
    Primitive (..),
    getAnnotationForType,
    varsFromDataType,
  )
where

import qualified Data.Aeson as JSON
import Data.Map (Map)
import qualified Data.Map as M
import Data.OpenApi
import Data.Text.Prettyprint.Doc
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST.Annotation
import Language.Mimsa.Types.Identifiers

data Primitive
  = MTInt
  | MTString
  | MTBool
  deriving stock
    ( Eq,
      Ord,
      Show,
      Generic
    )
  deriving anyclass
    ( JSON.ToJSON,
      JSON.FromJSON,
      ToSchema
    )

instance Printer Primitive where
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
  | MTRecordRow ann (Map Name (Type ann)) (Type ann) -- { foo:a, bar:b | rest }
  | MTArray ann (Type ann) -- [a]
  | MTConstructor ann TyCon -- name
  | MTTypeApp ann (Type ann) (Type ann) -- func arg, apply arg to func
  deriving stock (Eq, Ord, Show, Functor, Generic)
  deriving anyclass (JSON.ToJSON, JSON.FromJSON)

deriving anyclass instance (ToSchema ann) => ToSchema (Type ann)

getAnnotationForType :: Type ann -> ann
getAnnotationForType (MTPrim ann _) = ann
getAnnotationForType (MTVar ann _) = ann
getAnnotationForType (MTFunction ann _ _) = ann
getAnnotationForType (MTPair ann _ _) = ann
getAnnotationForType (MTRecord ann _) = ann
getAnnotationForType (MTRecordRow ann _ _) = ann
getAnnotationForType (MTConstructor ann _) = ann
getAnnotationForType (MTArray ann _) = ann
getAnnotationForType (MTTypeApp ann _ _) = ann

instance Printer (Type ann) where
  prettyDoc = renderMonoType

renderMonoType :: Type ann -> Doc style
renderMonoType (MTPrim _ a) = prettyDoc a
renderMonoType (MTFunction _ a b) =
  withParens a <+> "->" <+> renderMonoType b
renderMonoType (MTPair _ a b) =
  "(" <> renderMonoType a <> "," <+> renderMonoType b <> ")"
renderMonoType (MTRecord _ as) =
  group $
    "{"
      <> nest
        2
        ( line
            <> mconcat
              ( punctuate
                  ("," <> line)
                  ( renderItem
                      <$> M.toList as
                  )
              )
        )
      <> line
      <> "}"
  where
    renderItem (Name k, v) = pretty k <> ":" <+> withParens v
renderMonoType (MTRecordRow _ as rest) =
  group $
    "{"
      <> nest
        2
        ( line
            <> mconcat
              ( punctuate
                  ("," <> line)
                  ( renderItem
                      <$> M.toList as
                  )
              )
        )
      <> line
      <> "|"
      <> space
      <> renderMonoType rest
      <> space
      <> "}"
  where
    renderItem (Name k, v) = pretty k <> ":" <+> withParens v
renderMonoType (MTArray _ a) = "[" <+> renderMonoType a <+> "]"
renderMonoType (MTVar _ a) = renderTypeIdentifier a
renderMonoType (MTConstructor _ (TyCon n)) =
  pretty n
renderMonoType mt@(MTTypeApp _ func arg) =
  case varsFromDataType mt of
    Just (TyCon n, vars) -> align $ sep ([pretty n] <> (withParens <$> vars))
    Nothing ->
      align $ sep [renderMonoType func, renderMonoType arg]

-- turn nested shit back into something easy to pretty print (ie, easy to
-- bracket)
varsFromDataType :: Type ann -> Maybe (TyCon, [Type ann])
varsFromDataType mt =
  let getInner mt' =
        case mt' of
          (MTConstructor _ tyCon) -> Just (tyCon, mempty)
          (MTTypeApp _ f a) -> (\(tyCon, vars) -> (tyCon, vars <> [a])) <$> getInner f
          _ -> Nothing
   in getInner mt

withParens :: Type ann -> Doc a
withParens ma@MTFunction {} = parens (renderMonoType ma)
withParens mta@MTTypeApp {} = parens (renderMonoType mta)
withParens other = renderMonoType other
