module Language.Mimsa.Transform.UseOptimisedDeps where

-- given a store expression, update it use the bestest versions of it's deps
useOptimisedDeps ::
  Project Annotation ->
  StoreExpression Annotation ->
  StoreExpression Annotation
useOptimisedDeps proj se =
  let lookupOpt exprHash = fromMaybe exprHash (lookupOptimised proj exprHash)
   in se
        { bindings = lookupOpt <$> bindings se,
          typeBindings = lookupOpt <$> typeBindings se
        }
