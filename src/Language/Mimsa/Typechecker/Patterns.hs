module Language.Mimsa.Typechecker.Patterns where

import Control.Monad.Except
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Set as S
import Language.Mimsa.Typechecker.Environment
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Types

checkCompleteness ::
  Environment ->
  NonEmpty (Construct, Expr Variable) ->
  Maybe (Expr Variable) ->
  TcMonad DataType
checkCompleteness env opts catchAll = do
  -- find data type for each match
  items <- traverse (\(name, _) -> lookupConstructor env name) opts
  let optionNames = fst <$> (NE.toList opts)
  -- check they are all the same one
  dataType <- case nub (NE.toList items) of
    [a] -> pure a
    _ -> throwError (MixedUpPatterns optionNames)
  case catchAll of
    Just _ -> pure ()
    _ -> allPatternsExist optionNames dataType
  pure dataType

allPatternsExist :: [Construct] -> DataType -> TcMonad ()
allPatternsExist optNames' (DataType _ _ dataTypes) = do
  -- check each one of optNames exists in dataTypes
  let dtNames = S.fromList (M.keys dataTypes)
      optNames = S.fromList optNames'
  let (_matched, unmatched) = S.partition (`S.member` optNames) dtNames
  if S.size unmatched > 0
    then throwError (IncompletePatternMatch (S.toList unmatched))
    else pure ()
