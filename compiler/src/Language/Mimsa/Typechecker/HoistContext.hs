module Language.Mimsa.Typechecker.HoistContext where

import Data.Map (Map)
import qualified Data.Map as M
import Language.Mimsa.ExprUtils
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker

mtBool :: MonoType
mtBool = MTPrim mempty MTBool

addCtx :: MonoType -> Map Name MonoType -> MonoType
addCtx mt items | M.null items = mt
addCtx (MTContext ann existingItems restMt) items =
  MTContext ann (existingItems <> items) restMt
addCtx mt items =
  let existingAnn = getAnnotationForType mt
   in MTContext existingAnn items mt

-- chop out any contexts and put them on the top of the expr
hoistContext :: Expr var MonoType -> Expr var MonoType
hoistContext expr =
  let (newExpr, newCtx) = hoistContext' expr
   in mapOuterExprAnnotation (`addCtx` newCtx) newExpr
  where
    hoistContext' :: Expr var MonoType -> (Expr var MonoType, Map Name MonoType)
    hoistContext' (MyLet mt ident letExpr letBody) =
      let (letExpr', inner1) = hoistContext' letExpr
          (letBody', inner2) = hoistContext' letBody
          newCtx = inner1 <> inner2
       in (MyLet (addCtx mt newCtx) ident letExpr' letBody', newCtx)
    hoistContext' (MyFromContext mt name) =
      let (newCtx, newInner) = case mt of
            (MTContext _ items actualMt) -> (items, actualMt)
            _ -> (mempty, mt)
       in (MyFromContext newInner name, newCtx)
    hoistContext' other = (other, mempty)
