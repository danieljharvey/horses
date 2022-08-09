module Language.Mimsa.Store.Substitutor (getExprPairs) where

import Control.Monad (join)
import qualified Data.Map.Strict as M
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store

getExprPairs :: Store ann -> Bindings -> [(Name, StoreExpression ann)]
getExprPairs (Store items') (Bindings bindings') = join $ do
  (name, hash) <- M.toList bindings'
  case M.lookup hash items' of
    Just item -> pure [(name, item)]
    _ -> pure []
