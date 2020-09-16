module Language.Mimsa.Store.ExtractVars
  ( extractVars,
  )
where

import qualified Data.List.NonEmpty as NE
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Library
import Language.Mimsa.Types

-- important - we must not count variables brought in via lambdas, as those
-- aren't external deps

extractVars :: Expr Name -> Set Name
extractVars = filterBuiltIns . extractVars_

extractVars_ :: Expr Name -> Set Name
extractVars_ (MyVar a) = S.singleton a
extractVars_ (MyIf a b c) = extractVars_ a <> extractVars_ b <> extractVars_ c
extractVars_ (MyLet newVar a b) = S.delete newVar (extractVars_ a <> extractVars_ b)
extractVars_ (MyLambda newVar a) = S.delete newVar (extractVars_ a)
extractVars_ (MyApp a b) = extractVars_ a <> extractVars_ b
extractVars_ (MyLiteral _) = mempty
extractVars_ (MyLetPair newVarA newVarB a b) =
  S.delete
    newVarA
    (S.delete newVarB (extractVars_ a <> extractVars_ b))
extractVars_ (MyPair a b) = extractVars_ a <> extractVars_ b
extractVars_ (MyRecord map') = foldMap extractVars_ map'
extractVars_ (MyRecordAccess a _) = extractVars_ a
extractVars_ (MyData _ a) = extractVars_ a
extractVars_ (MyConstructor _) = mempty
extractVars_ (MyConsApp a b) = extractVars_ a <> extractVars_ b
extractVars_ (MyCaseMatch sum' matches catchAll) =
  extractVars sum'
    <> mconcat (extractVars . snd <$> NE.toList matches)
    <> maybe mempty extractVars catchAll

filterBuiltIns :: Set Name -> Set Name
filterBuiltIns = S.filter (not . isLibraryName)
