module Language.Mimsa.Store.ExtractVars
  ( extractVars,
  )
where

import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers

-- important - we must not count variables brought in via lambdas, as those
-- aren't external deps

extractVars :: (Eq ann, Monoid ann) => Expr Name ann -> Set Name
extractVars = extractVars_

extractVars_ :: (Eq ann, Monoid ann) => Expr Name ann -> Set Name
extractVars_ (MyVar _ _ a) = S.singleton a
extractVars_ (MyAnnotation _ _ expr) =
  extractVars_ expr
extractVars_ (MyIf _ a b c) =
  extractVars_ a <> extractVars_ b <> extractVars_ c
extractVars_ (MyLet _ ident a b) =
  S.difference (extractVars_ a <> extractVars_ b) (extractIdentVars ident)
extractVars_ (MyLetPattern _ pat expr body) =
  let patVars = extractPatternVars pat
   in S.filter (`S.notMember` patVars) (extractVars_ expr <> extractVars_ body)
extractVars_ (MyInfix _ _ a b) =
  extractVars_ a <> extractVars_ b
extractVars_ (MyLambda _ ident a) =
  S.difference (extractVars_ a) (extractIdentVars ident)
extractVars_ (MyApp _ a b) = extractVars_ a <> extractVars_ b
extractVars_ (MyLiteral _ _) = mempty
extractVars_ (MyPair _ a b) = extractVars_ a <> extractVars_ b
extractVars_ (MyRecord _ map') = foldMap extractVars_ map'
extractVars_ (MyRecordAccess _ a _) = extractVars_ a
extractVars_ (MyArray _ map') = foldMap extractVars_ map'
extractVars_ MyConstructor {} = mempty
extractVars_ (MyTypedHole _ _) = mempty
extractVars_ (MyPatternMatch _ match patterns) =
  extractVars match <> mconcat patternVars
  where
    patternVars :: [Set Name]
    patternVars =
      ( \(pat, expr) ->
          let patVars = extractPatternVars pat
           in S.filter (`S.notMember` patVars) (extractVars expr)
      )
        <$> patterns

extractIdentVars :: Identifier Name ann -> Set Name
extractIdentVars (Identifier _ name) = S.singleton name

extractPatternVars :: (Eq ann, Monoid ann) => Pattern Name ann -> Set Name
extractPatternVars (PWildcard _) = mempty
extractPatternVars (PLit _ _) = mempty
extractPatternVars (PVar _ a) = S.singleton a
extractPatternVars (PRecord _ as) =
  mconcat (extractPatternVars <$> M.elems as)
extractPatternVars (PPair _ a b) =
  extractPatternVars a <> extractPatternVars b
extractPatternVars (PConstructor _ _ _ args) =
  mconcat (extractPatternVars <$> args)
extractPatternVars (PArray _ as spread) =
  mconcat (extractPatternVars <$> as) <> extractSpreadVars spread
extractPatternVars (PString _ a as) =
  extractStringPart a <> extractStringPart as

extractSpreadVars :: Spread Name ann -> Set Name
extractSpreadVars NoSpread = mempty
extractSpreadVars (SpreadWildcard _) = mempty
extractSpreadVars (SpreadValue _ a) = S.singleton a

extractStringPart :: StringPart Name ann -> Set Name
extractStringPart (StrWildcard _) = mempty
extractStringPart (StrValue _ a) = S.singleton a
