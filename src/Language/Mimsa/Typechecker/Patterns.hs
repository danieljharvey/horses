module Language.Mimsa.Typechecker.Patterns where

import Control.Monad.Except
import Data.List (nub)
import Data.Map (Map)
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
  pure ()

allPatternsExist :: [Construct] -> (Construct, ([Name], Map Construct [TypeName])) -> TcMonad ()
allPatternsExist optNames (_, (_, dataTypes)) = do
  -- check each one of optNames exists in dataTypes
  pure ()
