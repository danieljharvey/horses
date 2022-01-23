module Language.Mimsa.Transform.TrimDeps where

import qualified Data.Map as M
import qualified Data.Set as S
import Language.Mimsa.Transform.FindUnused
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
        Bindings $
          M.filterWithKey
            (\k _ -> S.member k vars)
            (getBindings (storeBindings se))
   in se {storeExpression = newExpr, storeBindings = newBindings}
