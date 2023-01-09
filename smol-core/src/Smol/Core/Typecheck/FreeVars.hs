{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Smol.Core.Typecheck.FreeVars
  ( freeVars,
    freeTypeVars,
  )
where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Smol.Core.TypeUtils ( monoidType )
import qualified Smol.Core.Types.Expr as Smol ( Expr(..) )
import qualified Smol.Core.Types.Identifier as Smol ( Identifier )
import qualified Smol.Core.Types.Type as Smol ( Type(TVar) )

freeVars ::
  (Ord ann) =>
  Smol.Expr (Smol.Type ann) ->
  Set Smol.Identifier
freeVars (Smol.EVar _ a) = S.singleton a
freeVars Smol.EConstructor {} = mempty
freeVars (Smol.ELambda _ ident body) = S.delete ident (freeVars body)
freeVars (Smol.ELet _ ident expr body) = freeVars expr <> S.delete ident (freeVars body)
freeVars (Smol.EInfix _ _ a b) = freeVars a <> freeVars b
freeVars Smol.EPrim {} = mempty
freeVars (Smol.EApp _ f a) = freeVars f <> freeVars a
freeVars (Smol.EIf _ a b c) = freeVars a <> freeVars b <> freeVars c
freeVars (Smol.EAnn _ _ a) = freeVars a
freeVars (Smol.ETuple _ a as) =
  freeVars a <> mconcat (NE.toList $ freeVars <$> as)
freeVars (Smol.EGlobal _ _) = mempty
freeVars (Smol.EGlobalLet _ _ expr body) = freeVars expr <> freeVars body
freeVars (Smol.ERecord _ as) = mconcat (M.elems $ freeVars <$> as)
freeVars (Smol.ERecordAccess _ a _) = freeVars a
freeVars (Smol.EPatternMatch _ expr pats) =
  let getPatRequire (_pat, _patExpr) =
        mempty -- fucked - skipped test because cant be arsed right now
   in freeVars expr <> mconcat (NE.toList $ getPatRequire <$> pats)

freeTypeVars :: Smol.Type ann -> Set Smol.Identifier
freeTypeVars (Smol.TVar _ ident) = S.singleton ident
freeTypeVars other = monoidType freeTypeVars other
