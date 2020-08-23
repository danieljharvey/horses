module Language.Mimsa.Typechecker.Environment where

import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as M
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Types

-- given a constructor name, return the type it lives in
lookupConstructor ::
  Environment ->
  Construct ->
  TcMonad (Construct, ([Name], Map Construct [TypeName]))
lookupConstructor env name =
  let hasMatchingConstructor (_, items) = M.member name items
   in case M.toList $ M.filter hasMatchingConstructor (getDataTypes env) of
        [a] -> pure a -- we only want a single match
        (_ : _) -> throwError (ConflictingConstructors name)
        _ -> throwError (TypeConstructorNotInScope env name)
