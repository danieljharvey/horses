{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Smol.Core.Types.Type
  ( Type (..),
    TypePrim (..),
    TypeLiteral (..),
    renderType,
  )
where

import qualified Data.List.NonEmpty as NE
import Data.Map.Strict
import qualified Data.Map.Strict as M
import Prettyprinter ((<+>))
import qualified Prettyprinter as PP
import Smol.Core.Printer
import Smol.Core.Types.Identifier
import Smol.Core.Types.TypeName

data TypePrim = TPNat | TPInt | TPBool
  deriving stock (Eq, Ord, Show)

instance Printer TypePrim where
  prettyDoc TPNat = "Nat"
  prettyDoc TPInt = "Int"
  prettyDoc TPBool = "Bool"

data TypeLiteral = TLBool Bool | TLInt Integer | TLUnit
  deriving stock (Eq, Ord, Show)

instance Printer TypeLiteral where
  prettyDoc (TLBool b) = PP.pretty b
  prettyDoc (TLInt i) = PP.pretty i
  prettyDoc TLUnit = "Unit"

data Type ann
  = TLiteral ann TypeLiteral
  | TPrim ann TypePrim
  | TFunc ann (Map Identifier (Type ann)) (Type ann) (Type ann)
  | TTuple ann (Type ann) (NE.NonEmpty (Type ann))
  | TVar ann Identifier
  | TUnknown ann Integer
  | TGlobals ann (Map Identifier (Type ann)) (Type ann)
  | TRecord ann (Map Identifier (Type ann))
  | TUnion ann (Type ann) (Type ann)
  | TApp ann (Type ann) (Type ann)
  | TConstructor ann TypeName
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Printer (Type ann) where
  prettyDoc = renderType

renderType :: Type ann -> PP.Doc style
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

renderRecord :: Map Identifier (Type ann) -> PP.Doc style
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
varsFromDataType :: Type ann -> Maybe (TypeName, [Type ann])
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

withParens :: Type ann -> PP.Doc a
withParens ma@TFunc {} = PP.parens (renderType ma)
withParens mta@TApp {} = PP.parens (renderType mta)
withParens other = renderType other
