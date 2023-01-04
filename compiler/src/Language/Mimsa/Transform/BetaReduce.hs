module Language.Mimsa.Transform.BetaReduce (betaReduce) where

import qualified Data.Map.Strict as M
import Language.Mimsa.Core
import Language.Mimsa.Transform.Shared

betaReduce :: (Eq ann, Eq var) => Expr var ann -> Expr var ann
betaReduce = repeatUntilEq betaReduceInternal

-- | turn (\x -> e) a into `let x = a in e`
betaReduceInternal :: Expr var ann -> Expr var ann
betaReduceInternal (MyApp ann (MyLambda _ann ident body) val) =
  let (var, ann') = detailsFromIdent ident
   in MyLet ann (Identifier ann' var) val (betaReduceInternal body)
betaReduceInternal (MyApp ann (MyAnnotation _ _ (MyLambda _ann ident body)) val) =
  let (var, ann') = detailsFromIdent ident
   in MyLet ann (Identifier ann' var) val (betaReduceInternal body)
betaReduceInternal (MyIf _ (MyLiteral _ (MyBool True)) thenExpr _) =
  betaReduceInternal thenExpr
betaReduceInternal (MyIf _ (MyLiteral _ (MyBool False)) _ elseExpr) =
  betaReduceInternal elseExpr
betaReduceInternal (MyRecordAccess ann myRec@(MyRecord _ as) name) =
  case M.lookup name as of
    Just inner -> inner
    _ -> MyRecordAccess ann (betaReduceInternal myRec) name
betaReduceInternal other = mapExpr betaReduceInternal other
