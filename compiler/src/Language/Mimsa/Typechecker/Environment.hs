{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Typechecker.Environment (lookupConstructor) where

import Control.Monad.Except
import qualified Data.Map as M
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker

-- given a constructor name, return the type it lives in
lookupConstructor ::
  (MonadError TypeError m) =>
  Environment ->
  Annotation ->
  TyCon ->
  m DataType
lookupConstructor env ann tyCon = do
  case M.toList $ M.filter (containsConstructor tyCon) (getDataTypes env) of
    [(_, a)] -> pure a -- we only want a single match
    (_ : _) -> throwError (ConflictingConstructors ann tyCon)
    _ -> throwError (TypeConstructorNotInScope env ann tyCon)

-- does this data type contain the given constructor?
containsConstructor :: TyCon -> DataType -> Bool
containsConstructor tyCon (DataType _tyName _tyVars constructors) =
  M.member tyCon constructors
