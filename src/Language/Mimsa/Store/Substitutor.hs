module Language.Mimsa.Store.Substitutor where

import Control.Monad.Trans.State.Lazy
import Data.Text (Text)
import Language.Mimsa.Types

-- this turns StoreExpressions back into expressions by substituting their
-- variables for the deps passed in
--
-- like the typechecker, as we go though, we replace the varibable names with
-- var0, var1, var2 etc so we don't have to care about scoping or collisions
--
-- we'll also store what our substitutions were for errors sake

type Swaps = [(Name, Name)]

type App = StateT Swaps (Either Text)

substitute :: Store -> StoreExpression -> Either Text (Swaps, StoreExpression, Store)
substitute _store' (StoreExpression _bindings' _expr') = do
  pure undefined
