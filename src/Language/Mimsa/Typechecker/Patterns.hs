module Language.Mimsa.Typechecker.Patterns where

import Control.Monad.Except
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Language.Mimsa.Typechecker.Environment
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Types

checkCompleteness ::
  Environment ->
  [(Construct, Expr Variable)] ->
  Maybe (Expr Variable) ->
  TcMonad ()
checkCompleteness _ _ (Just _) = pure ()
checkCompleteness env opts Nothing = do
  -- find data type for each match
  items <- traverse (\(name, _) -> lookupConstructor env name) opts
  -- check they are all the same one
  dataType <- case nub items of
    [a] -> pure a
    _ -> throwError (MixedUpPatterns (fst <$> opts))
  allPatternsExist (fst <$> opts) dataType

allPatternsExist :: [Construct] -> (Construct, ([Name], Map Construct [TypeName])) -> TcMonad ()
allPatternsExist optNames' (_, (_, dataTypes)) = do
  -- check each one of optNames exists in dataTypes
  let dtNames = S.fromList (M.keys dataTypes)
      optNames = S.fromList optNames'
  let (_matched, unmatched) = S.partition (\a -> S.member a optNames) dtNames
  if S.size unmatched > 0
    then throwError (IncompletePatternMatch (S.toList unmatched))
    else pure ()
