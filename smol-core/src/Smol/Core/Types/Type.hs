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
    LocalType(..),
    TypePrim (..),
    TypeLiteral (..),
    ParsedType,
    ParsedLocalType,
    ResolvedType,
    renderType,
  )
where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON)
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
import Smol.Core.Types.ParseDep
import Smol.Core.Types.ResolvedDep
import Smol.Core.Types.TypeName

type ParsedType ann = Type ParseDep ann

type ParsedLocalType ann = LocalType ParseDep ann

type ResolvedType ann = Type ResolvedDep ann

data TypePrim = TPNat | TPInt | TPBool | TPString
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Printer TypePrim where
  prettyDoc TPNat = "Nat"
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

-- | type, minus globals
data LocalType dep ann
  = TLiteral ann TypeLiteral
  | TPrim ann TypePrim
  | TFunc ann (Map Identifier (LocalType dep ann)) (LocalType dep ann) (LocalType dep ann)
  | TTuple ann (LocalType dep ann) (NE.NonEmpty (LocalType dep ann))
  | TArray ann Word64 (LocalType dep ann)
  | TVar ann (dep Identifier)
  | TUnknown ann Integer
  | TRecord ann (Map Identifier (LocalType dep ann))
  | TApp ann (LocalType dep ann) (LocalType dep ann)
  | TConstructor ann (dep TypeName)
  deriving stock (Functor, Foldable, Generic, Traversable)

deriving stock instance
  ( Eq ann,
    Eq (dep Identifier),
    Eq (dep TypeName)
  ) =>
  Eq (LocalType dep ann)

deriving stock instance
  ( Ord ann,
    Ord (dep Identifier),
    Ord (dep TypeName)
  ) =>
  Ord (LocalType dep ann)

deriving stock instance
  ( Show ann,
    Show (dep Identifier),
    Show (dep TypeName)
  ) =>
  Show (LocalType dep ann)

deriving anyclass instance
  ( FromJSON ann,
    FromJSON (dep Identifier),
    FromJSON (dep TypeName)
  ) =>
  FromJSON (LocalType dep ann)

deriving anyclass instance
  ( ToJSON ann,
    ToJSON (dep Identifier),
    ToJSON (dep TypeName)
  ) =>
  ToJSON (LocalType dep ann)

deriving anyclass instance
  ( FromJSON ann,
    FromJSON (dep Identifier),
    FromJSON (dep TypeName),
    FromJSONKey ann,
    FromJSONKey (dep Identifier),
    FromJSONKey (dep TypeName)
  ) =>
  FromJSONKey (LocalType dep ann)

instance (Printer (dep Identifier), Printer (dep TypeName)) => Printer (LocalType dep ann) where
  prettyDoc = renderLocalType

-- | types are split this way so we only ever end up with one set of globals on
-- top, and they never have globals inside their types
data Type dep ann
  =
   TGlobals ann (Map Identifier (LocalType dep ann)) (LocalType dep ann)
  | TLocal (LocalType dep ann)
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
  ( FromJSON ann,
    FromJSON (dep Identifier),
    FromJSON (dep TypeName)
  ) =>
  FromJSON (Type dep ann)

deriving anyclass instance
  ( ToJSON ann,
    ToJSON (dep Identifier),
    ToJSON (dep TypeName)
  ) =>
  ToJSON (Type dep ann)

deriving anyclass instance
  ( FromJSON ann,
    FromJSON (dep Identifier),
    FromJSON (dep TypeName),
    FromJSONKey ann,
    FromJSONKey (dep Identifier),
    FromJSONKey (dep TypeName)
  ) =>
  FromJSONKey (Type dep ann)

instance (Printer (dep Identifier), Printer (dep TypeName)) => Printer (Type dep ann) where
  prettyDoc = renderType

renderType :: (Printer (dep Identifier),
 Printer (dep TypeName)) => Type dep ann -> PP.Doc style
renderType (TGlobals _ parts expr) =
  renderRecord parts <> " => " <> renderLocalType expr
renderType (TLocal inner) = renderLocalType inner

renderLocalType ::
  ( Printer (dep Identifier),
    Printer (dep TypeName)
  ) =>
  LocalType dep ann ->
  PP.Doc style
renderLocalType (TPrim _ a) = prettyDoc a
renderLocalType (TLiteral _ l) = prettyDoc l
renderLocalType (TUnknown _ i) = "U" <> PP.pretty i
renderLocalType (TArray _ _ as) = "[" <> renderLocalType as <> "]"
renderLocalType (TFunc _ _ a b) =
  withParens a <+> "->" <+> renderLocalType b
renderLocalType (TTuple _ a as) =
  "(" <> PP.hsep (PP.punctuate "," (renderLocalType <$> ([a] <> NE.toList as))) <> ")"
renderLocalType (TRecord _ as) =
  renderRecord as
-- renderLocalType (TArray _ a) = "[" <+> renderLocalType a <+> "]"
renderLocalType (TVar _ a) = prettyDoc a
renderLocalType (TConstructor _ tyCon) =
  prettyDoc tyCon
renderLocalType mt@(TApp _ func arg) =
  case varsFromDataType mt of
    Just (tyCon, vars) ->
      let typeName = prettyDoc tyCon
       in PP.align $ PP.sep ([typeName] <> (withParens <$> vars))
    Nothing ->
      PP.align $ PP.sep [renderLocalType func, renderLocalType arg]

renderRecord ::
  (Printer (dep Identifier), Printer (dep TypeName)) =>
  Map Identifier (LocalType dep ann) ->
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
varsFromDataType :: LocalType dep ann -> Maybe (dep TypeName, [LocalType dep ann])
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

withParens :: (Printer (dep Identifier),
    Printer (dep TypeName)) => LocalType dep ann -> PP.Doc a
withParens ma@TFunc {} = PP.parens (renderLocalType ma)
withParens mta@TApp {} = PP.parens (renderLocalType mta)
withParens other = renderLocalType other
