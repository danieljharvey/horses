module Language.Mimsa.Typechecker.Generalise
  ( generalise,
    freeTypeVars,
    freeTypeVarsCtx,
  )
where

import Data.List ((\\))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker

freeTypeVars :: MonoType -> S.Set TypeIdentifier
freeTypeVars ty = case ty of
  MTVar _ var ->
    S.singleton var
  MTFunction _ t1 t2 ->
    S.union (freeTypeVars t1) (freeTypeVars t2)
  MTPair _ t1 t2 -> S.union (freeTypeVars t1) (freeTypeVars t2)
  MTRecord _ as Nothing -> foldr S.union mempty (freeTypeVars <$> as)
  MTRecord _ as (Just rest) ->
    foldr S.union mempty (freeTypeVars <$> as)
      <> freeTypeVars rest
  MTArray _ a -> freeTypeVars a
  MTPrim _ _ -> S.empty
  MTConstructor {} -> S.empty
  MTTypeApp _ a b -> freeTypeVars a <> freeTypeVars b

freeTypeVarsScheme :: Scheme -> [TypeIdentifier]
freeTypeVarsScheme (Scheme vars t) =
  S.toList (freeTypeVars t) \\ vars

freeTypeVarsCtx :: Environment -> [TypeIdentifier]
freeTypeVarsCtx (Environment env _ _ _ _) =
  foldMap freeTypeVarsScheme (M.elems env)

generalise :: Environment -> MonoType -> Scheme
generalise env ty =
  Scheme (S.toList $ S.fromList vars) ty
  where
    vars =
      S.toList (freeTypeVars ty) \\ freeTypeVarsCtx env
