module Language.Mimsa.Typechecker.Generalise
  ( generalise,
    freeTypeVars,
    freeTypeVarsCtx,
  )
where

import Data.List ((\\))
import qualified Data.HashMap.Strict as M
import qualified Data.Set as S
import Language.Mimsa.Core
import Language.Mimsa.Types.Typechecker

freeTypeVars :: MonoType -> S.Set TypeIdentifier
freeTypeVars ty = case ty of
  MTVar _ var ->
    S.singleton var
  other -> withMonoidType freeTypeVars other

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
