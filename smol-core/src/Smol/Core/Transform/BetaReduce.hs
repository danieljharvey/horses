module Smol.Core.Transform.BetaReduce (betaReduce) where

import qualified Data.Map.Strict as M
import Smol.Core.ExprUtils
import Smol.Core.Types

betaReduce :: Expr dep ann -> Expr dep ann
betaReduce = betaReduceInternal

-- | turn (\x -> e) a into `let x = a in e`
betaReduceInternal :: Expr dep ann -> Expr dep ann
betaReduceInternal (EApp ann (ELambda _ ident body) val) =
  betaReduceInternal $ ELet ann ident val (betaReduceInternal body)
betaReduceInternal (EApp ann (EAnn _ _ (ELambda _ann ident body)) val) =
  betaReduceInternal $ ELet ann ident val (betaReduceInternal body)
betaReduceInternal (EApp annA (EApp annB (ELambda _ identA (ELambda _ identB body)) valA) valB) =
  betaReduceInternal $ ELet annA identA valA (ELet annB identB valB (betaReduceInternal body))
betaReduceInternal (EIf _ (EPrim _ (PBool True)) thenExpr _) =
  betaReduceInternal thenExpr
betaReduceInternal (EIf _ (EPrim _ (PBool False)) _ elseExpr) =
  betaReduceInternal elseExpr
betaReduceInternal (ERecordAccess ann myRec@(ERecord _ as) name) =
  case M.lookup name as of
    Just inner -> inner
    _ -> ERecordAccess ann (betaReduceInternal myRec) name
betaReduceInternal other = mapExpr betaReduceInternal other
