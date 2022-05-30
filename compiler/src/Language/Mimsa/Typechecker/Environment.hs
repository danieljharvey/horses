{-# LANGUAGE FlexibleContexts #-}

module Language.Mimsa.Typechecker.Environment (lookupConstructor) where

import Data.Map (Map)
import Control.Monad.Except
import qualified Data.Map as M
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules.ModuleName
import Language.Mimsa.Types.Typechecker

-- given a constructor name, return the type it lives in
lookupConstructor ::
  (MonadError (TypeErrorF var Annotation) m) =>
  Environment ->
  Annotation ->
  Maybe ModuleName ->
  TyCon ->
  m DataType
lookupConstructor env ann modName name = 
  let dts = inMatchingNamespace modName (getDataTypes env) 
   in case M.toList $ M.filter (containsConstructor name) dts of
    [(_, a)] -> pure a -- we only want a single match
    (_ : _) -> throwError (ConflictingConstructors ann name)
    _ -> throwError (TypeConstructorNotInScope env ann modName name)

-- only look at stuff in the same namespace
inMatchingNamespace :: (Eq a) => a -> Map (a, b) c -> Map (a, b) c
inMatchingNamespace match = M.filterWithKey (\(k1,_) _ -> k1 == match) 

-- does this data type contain the given constructor?
containsConstructor :: TyCon -> DataType -> Bool
containsConstructor name (DataType _tyName _tyVars constructors) =
  M.member name constructors
