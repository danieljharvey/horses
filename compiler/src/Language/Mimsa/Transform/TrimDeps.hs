module Language.Mimsa.Transform.TrimDeps where

import qualified Data.Map as M
import Language.Mimsa.Transform.FindUses
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store

-- | given a store expression and updated expr
-- bin off any unused stuff
-- doesn't do types yet
trimDeps :: StoreExpression ann -> Expr Name ann -> StoreExpression ann
trimDeps se newExpr =
  let vars = findUses newExpr
      newBindings =
          M.filterWithKey
            (\(modName,k) _ -> memberInUses k modName vars)
            (storeBindings se)
   in se {storeExpression = newExpr, storeBindings = newBindings}
