module Language.Mimsa.Transform.UseOptimisedDeps (useOptimisedDeps) where

import Data.Maybe
import Language.Mimsa.Project
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.Store

-- given a store expression, update it use the bestest versions of it's deps
useOptimisedDeps ::
  Project Annotation ->
  StoreExpression Annotation ->
  StoreExpression Annotation
useOptimisedDeps proj se =
  let lookupOpt exprHash = fromMaybe exprHash (lookupOptimised proj exprHash)
   in se
        { storeBindings =
            Bindings $
              lookupOpt
                <$> getBindings (storeBindings se),
          storeTypeBindings =
            TypeBindings $
              lookupOpt
                <$> getTypeBindings (storeTypeBindings se)
        }
