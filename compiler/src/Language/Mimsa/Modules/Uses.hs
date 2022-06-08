{-# LANGUAGE LambdaCase #-}

module Language.Mimsa.Modules.Uses
  ( extractUses,
  )
where

import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.TypeUtils (withMonoid)
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules.Entity
import Language.Mimsa.Types.Typechecker

-- find all uses of external vars, types, infix operators etc
-- used in dependency analysis
-- important - we must not count variables brought in via lambdas or let
-- bindings as those aren't external deps

extractUses :: (Eq ann, Monoid ann) => Expr Name ann -> Set Entity
extractUses = extractUses_

extractUses_ :: (Eq ann, Monoid ann) => Expr Name ann -> Set Entity
extractUses_ (MyVar _ (Just modName) a) = S.singleton (ENamespacedName modName a)
extractUses_ (MyVar _ _ a) = S.singleton (EName a)
extractUses_ (MyAnnotation _ mt expr) =
  extractUses_ expr <> extractTypeUses mt
extractUses_ (MyIf _ a b c) =
  extractUses_ a <> extractUses_ b <> extractUses_ c
extractUses_ (MyLet _ ident a b) =
  S.difference (extractUses_ a <> extractUses_ b) (extractIdentUses ident)
extractUses_ (MyLetPattern _ pat expr body) =
  let patUses = extractPatternUses pat
   in filterVarsIntroducedInPatterns patUses 
            (extractUses_ expr <> extractUses_ body)
extractUses_ (MyInfix _ op a b) =
  let infixUses = case op of
        Custom infixOp -> S.singleton (EInfix infixOp)
        _ -> mempty
   in infixUses
        <> extractUses_ a
        <> extractUses_ b
extractUses_ (MyLambda _ ident a) =
  S.difference (extractUses_ a) (extractIdentUses ident)
extractUses_ (MyApp _ a b) = extractUses_ a <> extractUses_ b
extractUses_ (MyLiteral _ _) = mempty
extractUses_ (MyPair _ a b) = extractUses_ a <> extractUses_ b
extractUses_ (MyRecord _ map') = foldMap extractUses_ map'
extractUses_ (MyRecordAccess _ a _) = extractUses_ a
extractUses_ (MyArray _ map') = foldMap extractUses_ map'
extractUses_ (MyData _ _ a) = extractUses_ a
extractUses_ (MyConstructor _ (Just modName) tyCon) =
  S.singleton (ENamespacedConstructor modName tyCon)
extractUses_ (MyConstructor _ Nothing tyCon) =
  S.singleton (EConstructor tyCon)
extractUses_ (MyTypedHole _ _) = mempty
extractUses_ (MyDefineInfix _ _ a b) = extractUses_ a <> extractUses_ b
extractUses_ (MyPatternMatch _ match patterns) =
  extractUses match <> mconcat patternUses
  where
    patternUses :: [Set Entity]
    patternUses =
      ( \(pat, expr) ->
          filterVarsIntroducedInPatterns
            (extractPatternUses pat)
            (extractUses expr)
      )
        <$> patterns

-- for vars, remove any vars introduced in patterns in the expressions
-- for everything else, keep both
filterVarsIntroducedInPatterns :: Set Entity -> Set Entity -> Set Entity
filterVarsIntroducedInPatterns patUses exprUses =
  let patVarUses =
        S.filter
          ( \case
              EName _ -> True
              _ -> False
          )
          patUses
   in S.filter (`S.notMember` patVarUses) (patUses <> exprUses)

extractIdentUses :: Identifier Name ann -> Set Entity
extractIdentUses (Identifier _ name) = S.singleton (EName name)

extractPatternUses :: (Eq ann, Monoid ann) => Pattern Name ann -> Set Entity
extractPatternUses (PWildcard _) = mempty
extractPatternUses (PLit _ _) = mempty
extractPatternUses (PVar _ a) = S.singleton (EName a)
extractPatternUses (PRecord _ as) =
  mconcat (extractPatternUses <$> M.elems as)
extractPatternUses (PPair _ a b) =
  extractPatternUses a <> extractPatternUses b
extractPatternUses (PConstructor _ maybeMod tyCon args) =
  let modEntity = case maybeMod of
        Just modName -> S.singleton (ENamespacedConstructor modName tyCon)
        _ -> S.singleton (EConstructor tyCon)
   in modEntity <> mconcat (extractPatternUses <$> args)
extractPatternUses (PArray _ as spread) =
  mconcat (extractPatternUses <$> as) <> extractSpreadUses spread
extractPatternUses (PString _ a as) =
  extractStringPart a <> extractStringPart as

extractSpreadUses :: Spread Name ann -> Set Entity
extractSpreadUses NoSpread = mempty
extractSpreadUses (SpreadWildcard _) = mempty
extractSpreadUses (SpreadValue _ a) = S.singleton (EName a)

extractStringPart :: StringPart Name ann -> Set Entity
extractStringPart (StrWildcard _) = mempty
extractStringPart (StrValue _ a) = S.singleton (EName a)

-- extract uses in a type
extractTypeUses :: Type ann -> Set Entity
extractTypeUses (MTConstructor _ (Just modName) typeName) =
  S.singleton (ENamespacedType modName typeName)
extractTypeUses (MTConstructor _ Nothing typeName) =
  S.singleton (EType typeName)
extractTypeUses other = withMonoid extractTypeUses other
