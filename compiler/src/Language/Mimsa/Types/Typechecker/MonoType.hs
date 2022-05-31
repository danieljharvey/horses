{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

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
import GHC.Generics
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST.Annotation
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Identifiers.TypeName
import Language.Mimsa.Types.Modules.ModuleName
import Prettyprinter

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
      JSON.FromJSON
    )

instance Printer Primitive where
  prettyDoc MTInt = "Int"
  prettyDoc MTString = "String"
  prettyDoc MTBool = "Boolean"

type MonoType = Type Annotation

data Type ann
  = MTPrim
      { typAnn :: ann,
        typPrim :: Primitive
      }
  | MTVar
      { typAnn :: ann,
        typIdent :: TypeIdentifier
      }
  | MTFunction
      { typAnn :: ann,
        typArg :: Type ann,
        typRes :: Type ann -- argument, result
      }
  | MTPair
      { typAnn :: ann,
        typA :: Type ann,
        typB :: Type ann -- (a,b)
      }
  | MTRecord
      { typAnn :: ann,
        typRecordItems :: Map Name (Type ann) -- { foo: a, bar: b }
      }
  | MTRecordRow
      { typAnn :: ann,
        typRecordItems :: Map Name (Type ann),
        typRest :: Type ann -- { foo:a, bar:b | rest }
      }
  | MTArray
      { typAnn :: ann,
        typArrayItems :: Type ann -- [a]
      }
  | MTConstructor
      { typAnn :: ann,
        typModuleName :: Maybe ModuleName,
        typTypeName :: TypeName -- name
      }
  | MTTypeApp
      { typAnn :: ann,
        typFunc :: Type ann,
        typArg :: Type ann -- func arg, apply arg to func
      }
  deriving stock (Eq, Ord, Show, Functor, Foldable, Generic)
  deriving anyclass (JSON.ToJSON, JSON.FromJSON)

getAnnotationForType :: Type ann -> ann
getAnnotationForType (MTPrim ann _) = ann
getAnnotationForType (MTVar ann _) = ann
getAnnotationForType (MTFunction ann _ _) = ann
getAnnotationForType (MTPair ann _ _) = ann
getAnnotationForType (MTRecord ann _) = ann
getAnnotationForType (MTRecordRow ann _ _) = ann
getAnnotationForType (MTConstructor ann _ _) = ann
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
renderMonoType (MTConstructor _ (Just modName) tyCon) =
  prettyDoc modName <> "." <> prettyDoc tyCon
renderMonoType (MTConstructor _ Nothing tyCon) =
  prettyDoc tyCon
renderMonoType mt@(MTTypeApp _ func arg) =
  case varsFromDataType mt of
    Just (modName, tyCon, vars) ->
      let typeName = case modName of
            Just mName -> prettyDoc mName <> "." <> prettyDoc tyCon
            _ -> prettyDoc tyCon
       in align $ sep ([typeName] <> (withParens <$> vars))
    Nothing ->
      align $ sep [renderMonoType func, renderMonoType arg]

-- turn nested shit back into something easy to pretty print (ie, easy to
-- bracket)
varsFromDataType :: Type ann -> Maybe (Maybe ModuleName, TypeName, [Type ann])
varsFromDataType mt =
  let getInner mt' =
        case mt' of
          (MTConstructor _ modName tyCon) ->
            Just (modName, tyCon, mempty)
          (MTTypeApp _ f a) ->
            ( \(modName, tyCon, vars) ->
                (modName, tyCon, vars <> [a])
            )
              <$> getInner f
          _ -> Nothing
   in getInner mt

withParens :: Type ann -> Doc a
withParens ma@MTFunction {} = parens (renderMonoType ma)
withParens mta@MTTypeApp {} = parens (renderMonoType mta)
withParens other = renderMonoType other
