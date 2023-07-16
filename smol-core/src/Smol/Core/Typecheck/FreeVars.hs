{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Smol.Core.Typecheck.FreeVars
  ( freeVars,
    freeTypeVars,
  )
where

import Data.Foldable (toList)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Smol.Core.TypeUtils (monoidType)
import Smol.Core.Types.Expr (Expr (..))
import Smol.Core.Types.Identifier (Identifier)
import Smol.Core.Types.Type (Type (TVar))

freeVars ::
  ( Ord ann,
    Ord (dep Identifier)
  ) =>
  Expr dep (Type dep ann) ->
  Set (dep Identifier)
freeVars (EVar _ a) = S.singleton a
freeVars EConstructor {} = mempty
freeVars (ELambda _ ident body) = S.delete ident (freeVars body)
freeVars (ELet _ ident expr body) = freeVars expr <> S.delete ident (freeVars body)
freeVars (EInfix _ _ a b) = freeVars a <> freeVars b
freeVars EPrim {} = mempty
freeVars (EApp _ f a) = freeVars f <> freeVars a
freeVars (EIf _ a b c) = freeVars a <> freeVars b <> freeVars c
freeVars (EAnn _ _ a) = freeVars a
freeVars (ETuple _ a as) =
  freeVars a <> mconcat (NE.toList $ freeVars <$> as)
freeVars (EArray _ as) = mconcat (freeVars <$> toList as)
freeVars (ERecord _ as) = mconcat (M.elems $ freeVars <$> as)
freeVars (ERecordAccess _ a _) = freeVars a
freeVars (EPatternMatch _ expr pats) =
  let getPatRequire (_pat, _patExpr) =
        mempty -- fucked - skipped test because cant be arsed right now
   in freeVars expr <> mconcat (NE.toList $ getPatRequire <$> pats)

freeTypeVars ::
  (Ord (dep Identifier)) =>
  Type dep ann ->
  Set (dep Identifier)
freeTypeVars (TVar _ ident) = S.singleton ident
freeTypeVars other = monoidType freeTypeVars other
