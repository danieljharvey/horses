{-# LANGUAGE LambdaCase #-}

module Smol.Modules.Uses
  ( extractUses,
    extractUsesTyped,
    extractDataTypeUses,
    extractTypeUses,
  )
where

import Data.Foldable (toList)
import qualified Data.List.NonEmpty as NE
import Data.Set (Set)
import qualified Data.Set as S
import Smol.Core
import qualified Smol.Modules.Types.Entity as E

extractUses :: (Eq ann) => Expr ParseDep ann -> Set E.Entity
extractUses expr =
  let typeNames = dataTypeNames expr
   in S.filter
        (\ent -> not $ S.member ent typeNames)
        ( extractUses_ expr
        )

-- | extract uses in an expression annotated with types
extractUsesTyped :: (Eq ann) => Expr ParseDep (Type ParseDep ann) -> Set E.Entity
extractUsesTyped expr =
  let typeNames = dataTypeNames expr
   in S.filter
        (\ent -> not $ S.member ent typeNames)
        ( extractUses_ expr
            <> foldMap extractTypeUses expr
        )

-- | find all uses of external vars, types, infix operators etc
-- used in dependency analysis
-- important - we must not count variables brought in via lambdas or let
-- bindings as those aren't external deps
extractUses_ :: (Eq ann) => Expr ParseDep ann -> Set E.Entity
extractUses_ (EVar _ (ParseDep a (Just modName))) =
  S.singleton (E.ENamespacedVar modName a)
extractUses_ (EVar _ (ParseDep a Nothing)) =
  S.singleton (E.EVar a)
extractUses_ (EAnn _ mt expr) =
  extractUses_ expr <> extractTypeUses mt
extractUses_ (EArray _ as) = mconcat $ extractUses_ <$> toList as
extractUses_ (EIf _ a b c) =
  extractUses_ a <> extractUses_ b <> extractUses_ c
extractUses_ (ELet _ ident a b) =
  S.difference
    (extractUses_ a <> extractUses_ b)
    (extractIdentUses ident)
extractUses_ (EInfix _ _ a b) =
  extractUses_ a
    <> extractUses_ b
extractUses_ (ELambda _ ident a) =
  S.difference (extractUses_ a) (extractIdentUses ident)
extractUses_ (EApp _ a b) = extractUses_ a <> extractUses_ b
extractUses_ (EPrim _ _) = mempty
extractUses_ (ETuple _ a as) = extractUses_ a <> foldMap extractUses_ as
extractUses_ (ERecord _ map') = foldMap extractUses_ map'
extractUses_ (ERecordAccess _ a _) = extractUses_ a
extractUses_ (EConstructor _ (ParseDep tyCon (Just modName))) =
  S.singleton (E.ENamespacedConstructor modName tyCon)
extractUses_ (EConstructor _ (ParseDep tyCon Nothing)) =
  S.singleton (E.EConstructor tyCon)
extractUses_ (EPatternMatch _ match patterns) =
  extractUses match <> mconcat patternUses
  where
    patternUses :: [Set E.Entity]
    patternUses =
      ( \(pat, expr) ->
          filterVarsIntroducedInPatterns
            (extractPatternUses pat)
            (extractUses expr)
      )
        <$> NE.toList patterns

-- for vars, remove any vars introduced in patterns in the expressions
-- for everything else, keep both
filterVarsIntroducedInPatterns :: Set E.Entity -> Set E.Entity -> Set E.Entity
filterVarsIntroducedInPatterns patUses exprUses =
  let patVarUses =
        S.filter
          ( \case
              E.EVar _ -> True
              _ -> False
          )
          patUses
   in S.filter (`S.notMember` patVarUses) (patUses <> exprUses)

extractIdentUses :: ParseDep Identifier -> Set E.Entity
extractIdentUses (ParseDep name _) = S.singleton (E.EVar name)

extractPatternUses :: (Eq ann) => Pattern ParseDep ann -> Set E.Entity
extractPatternUses (PWildcard _) = mempty
extractPatternUses (PLiteral _ _) = mempty
extractPatternUses (PVar _ (ParseDep a _)) = S.singleton (E.EVar a)
extractPatternUses (PTuple _ a as) =
  extractPatternUses a <> foldMap extractPatternUses as
extractPatternUses (PArray _ as spread) =
  foldMap extractPatternUses as <> case spread of
    NoSpread -> mempty
    SpreadWildcard {} -> mempty
    SpreadValue _ (ParseDep a _) -> S.singleton (E.EVar a)
extractPatternUses (PConstructor _ (ParseDep tyCon maybeMod) args) =
  let modEntity = case maybeMod of
        Just modName -> S.singleton (E.ENamespacedConstructor modName tyCon)
        _ -> S.singleton (E.EConstructor tyCon)
   in modEntity <> mconcat (extractPatternUses <$> args)

-- extract uses in a type
extractTypeUses :: Type ParseDep ann -> Set E.Entity
extractTypeUses (TConstructor _ (ParseDep typeName (Just modName))) =
  S.singleton (E.ENamespacedType modName typeName)
extractTypeUses (TConstructor _ (ParseDep typeName Nothing)) =
  S.singleton (E.EType typeName)
extractTypeUses other = monoidType extractTypeUses other

-- | find other types used in the declaration of a datatype
extractDataTypeUses :: DataType ParseDep ann -> Set E.Entity
extractDataTypeUses (DataType typeName _ constructors) =
  S.filter
    (\entity -> entity /= E.EType typeName)
    ( foldMap (foldMap extractTypeUses) constructors
    )

dataTypeNames :: Expr ParseDep ann -> Set E.Entity
dataTypeNames (ELet _ _ expr body) = dataTypeNames expr <> dataTypeNames body
dataTypeNames _ = mempty
