module Language.Mimsa.Store.DepGraph where

import Data.Map ((!))
import qualified Data.Map as M
import Language.Mimsa.Types.Name
import Language.Mimsa.Types.Store

data DepGraph
  = Func Name [DepGraph]

createDepGraph :: Name -> Store -> StoreExpression -> DepGraph
createDepGraph name (Store store') storeExpr' = Func name leaves
  where
    leaves =
      ( \(name', hash) ->
          createDepGraph name' (Store store') (store' ! hash)
      )
        <$> children
    children = (M.toList . getBindings . storeBindings $ storeExpr')
