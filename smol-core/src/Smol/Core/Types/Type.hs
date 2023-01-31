{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Types.Type
  ( Type (..),
    TypePrim (..),
    TypeLiteral (..),
    ParsedType,
    ResolvedType,
    renderType,
  )
where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON)
import qualified Data.Kind as Kind
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict
import qualified Data.Map.Strict as M
import GHC.Generics (Generic)
import Prettyprinter ((<+>))
import qualified Prettyprinter as PP
import Smol.Core.Printer
import Smol.Core.Types.Identifier
import Smol.Core.Types.ParseDep
import Smol.Core.Types.ResolvedDep
import Smol.Core.Types.TypeName

type ParsedType ann = Type ParseDep ann

type ResolvedType ann = Type ResolvedDep ann

data TypePrim = TPNat | TPInt | TPBool
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Printer TypePrim where
  prettyDoc TPNat = "Nat"
  prettyDoc TPInt = "Int"
  prettyDoc TPBool = "Bool"

data TypeLiteral = TLBool Bool | TLInt Integer | TLUnit
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Printer TypeLiteral where
  prettyDoc (TLBool b) = PP.pretty b
  prettyDoc (TLInt i) = PP.pretty i
  prettyDoc TLUnit = "Unit"

data Type (dep :: Kind.Type -> Kind.Type) ann
  = TLiteral ann TypeLiteral
  | TPrim ann TypePrim
  | TFunc ann (Map Identifier (Type dep ann)) (Type dep ann) (Type dep ann)
  | TTuple ann (Type dep ann) (NE.NonEmpty (Type dep ann))
  | TVar ann Identifier
  | TUnknown ann Integer
  | TGlobals ann (Map Identifier (Type dep ann)) (Type dep ann)
  | TRecord ann (Map Identifier (Type dep ann))
  | TUnion ann (Type dep ann) (Type dep ann)
  | TApp ann (Type dep ann) (Type dep ann)
  | TConstructor ann TypeName
  deriving stock (Eq, Ord, Show, Functor, Foldable, Generic, Traversable)
  deriving anyclass (FromJSON, FromJSONKey, ToJSON)

instance Printer (Type dep ann) where
  prettyDoc = renderType

renderType :: Type dep ann -> PP.Doc style
renderType (TPrim _ a) = prettyDoc a
renderType (TLiteral _ l) = prettyDoc l
renderType (TUnknown _ i) = "U" <> PP.pretty i
renderType (TUnion _ a b) = prettyDoc a <+> "|" <+> prettyDoc b
renderType (TFunc _ _ a b) =
  withParens a <+> "->" <+> renderType b
renderType (TTuple _ a as) =
  "(" <> PP.hsep (PP.punctuate "," (renderType <$> ([a] <> NE.toList as))) <> ")"
renderType (TRecord _ as) =
  renderRecord as
-- renderType (TArray _ a) = "[" <+> renderType a <+> "]"
renderType (TVar _ a) = prettyDoc a
renderType (TConstructor _ tyCon) =
  prettyDoc tyCon
renderType mt@(TApp _ func arg) =
  case varsFromDataType mt of
    Just (tyCon, vars) ->
      let typeName = prettyDoc tyCon
       in PP.align $ PP.sep ([typeName] <> (withParens <$> vars))
    Nothing ->
      PP.align $ PP.sep [renderType func, renderType arg]
renderType (TGlobals _ parts expr) =
  renderRecord parts <> " => " <> renderType expr

renderRecord :: Map Identifier (Type dep ann) -> PP.Doc style
renderRecord as =
  PP.group $
    "{"
      <> PP.nest
        2
        ( PP.line
            <> mconcat
              ( PP.punctuate
                  ("," <> PP.line)
                  ( renderItem
                      <$> M.toList as
                  )
              )
        )
      <> PP.line
      <> "}"
  where
    renderItem (k, v) = prettyDoc k <> ":" <+> withParens v

-- turn nested shit back into something easy to pretty print (ie, easy to
-- bracket)
varsFromDataType :: Type dep ann -> Maybe (TypeName, [Type dep ann])
varsFromDataType mt =
  let getInner mt' =
        case mt' of
          (TConstructor _ tyCon) ->
            Just (tyCon, mempty)
          (TApp _ f a) ->
            ( \(tyCon, vars) ->
                (tyCon, vars <> [a])
            )
              <$> getInner f
          _ -> Nothing
   in getInner mt

withParens :: Type dep ann -> PP.Doc a
withParens ma@TFunc {} = PP.parens (renderType ma)
withParens mta@TApp {} = PP.parens (renderType mta)
withParens other = renderType other
