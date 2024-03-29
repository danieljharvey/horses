{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Smol.Core.Types.Type
  ( Type (..),
    TypePrim (..),
    TypeLiteral (..),
    ParsedType,
    ResolvedType,
    renderType,
  )
where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Set.NonEmpty as NES
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Prettyprinter ((<+>))
import qualified Prettyprinter as PP
import Smol.Core.Printer
import Smol.Core.Types.Identifier
import Smol.Core.Types.Op
import Smol.Core.Types.ParseDep
import Smol.Core.Types.ResolvedDep
import Smol.Core.Types.TypeName

type ParsedType ann = Type ParseDep ann

type ResolvedType ann = Type ResolvedDep ann

data TypePrim = TPInt | TPBool | TPString
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Printer TypePrim where
  prettyDoc TPInt = "Int"
  prettyDoc TPBool = "Bool"
  prettyDoc TPString = "String"

data TypeLiteral
  = TLBool Bool
  | TLInt (NES.NESet Integer)
  | TLString (NES.NESet Text)
  | TLUnit
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Printer TypeLiteral where
  prettyDoc (TLBool b) = PP.pretty b
  prettyDoc (TLInt neInts) =
    PP.hsep (PP.punctuate "| " (PP.pretty <$> S.toList (NES.toSet neInts)))
  prettyDoc (TLString neStrs) =
    PP.hsep (PP.punctuate "| " (PP.pretty <$> S.toList (NES.toSet neStrs)))
  prettyDoc TLUnit = "Unit"

data Type dep ann
  = TLiteral ann TypeLiteral
  | TPrim ann TypePrim
  | TFunc ann (Map (dep Identifier) (Type dep ann)) (Type dep ann) (Type dep ann)
  | TTuple ann (Type dep ann) (NE.NonEmpty (Type dep ann))
  | TArray ann Word64 (Type dep ann)
  | TVar ann (dep Identifier)
  | TUnknown ann Integer
  | TRecord ann (Map Identifier (Type dep ann))
  | TApp ann (Type dep ann) (Type dep ann)
  | TConstructor ann (dep TypeName)
  | TInfix ann Op (Type dep ann) (Type dep ann)
  deriving stock (Functor, Foldable, Generic, Traversable)

deriving stock instance
  ( Eq ann,
    Eq (dep Identifier),
    Eq (dep TypeName)
  ) =>
  Eq (Type dep ann)

deriving stock instance
  ( Ord ann,
    Ord (dep Identifier),
    Ord (dep TypeName)
  ) =>
  Ord (Type dep ann)

deriving stock instance
  ( Show ann,
    Show (dep Identifier),
    Show (dep TypeName)
  ) =>
  Show (Type dep ann)

deriving anyclass instance
  ( Ord (dep Identifier),
    FromJSONKey (dep Identifier),
    FromJSON ann,
    FromJSON (dep Identifier),
    FromJSON (dep TypeName)
  ) =>
  FromJSON (Type dep ann)

deriving anyclass instance
  ( ToJSONKey (dep Identifier),
    ToJSON ann,
    ToJSON (dep Identifier),
    ToJSON (dep TypeName)
  ) =>
  ToJSON (Type dep ann)

deriving anyclass instance
  ( Ord (dep Identifier),
    FromJSON ann,
    FromJSON (dep Identifier),
    FromJSON (dep TypeName),
    FromJSONKey ann,
    FromJSONKey (dep Identifier),
    FromJSONKey (dep TypeName)
  ) =>
  FromJSONKey (Type dep ann)

instance (Printer (dep Identifier), Printer (dep TypeName)) => Printer (Type dep ann) where
  prettyDoc = renderType

renderType ::
  ( Printer (dep Identifier),
    Printer (dep TypeName)
  ) =>
  Type dep ann ->
  PP.Doc style
renderType (TPrim _ a) = prettyDoc a
renderType (TInfix _ op a b) = prettyDoc a <+> prettyDoc op <+> prettyDoc b
renderType (TLiteral _ l) = prettyDoc l
renderType (TUnknown _ i) = "U" <> PP.pretty i
renderType (TArray _ _ as) = "[" <> prettyDoc as <> "]"
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

renderRecord ::
  (Printer (dep Identifier), Printer (dep TypeName)) =>
  Map Identifier (Type dep ann) ->
  PP.Doc style
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
varsFromDataType :: Type dep ann -> Maybe (dep TypeName, [Type dep ann])
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

withParens :: (Printer (dep Identifier), Printer (dep TypeName)) => Type dep ann -> PP.Doc a
withParens ma@TFunc {} = PP.parens (renderType ma)
withParens mta@TApp {} = PP.parens (renderType mta)
withParens other = renderType other
