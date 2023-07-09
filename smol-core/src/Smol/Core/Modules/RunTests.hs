{-# LANGUAGE NamedFieldPuns #-}
module Smol.Core.Modules.RunTests (runTests) where

import Data.Functor (void)
import Control.Monad.Identity
import Smol.Core
import qualified Data.Map.Strict as M
import Smol.Core.Modules.Types

-- | We have a ResolvedDep with lots of info, but when it comes to compiling
-- we don't want to leak all that shit. `IdentityExpr` is no doubt the wrong
-- choice but fuck it
fromResolvedExpr :: ResolvedExpr ann -> IdentityExpr ann
fromResolvedExpr = mapExprDep resolve

doInterpret :: Expr ResolvedDep () -> Expr Identity ()
doInterpret =
  toExpr
    . runIdentity
    . interpret
    . fromExpr
    . fromResolvedExpr

-- for now, throw extra info away
resolve :: ResolvedDep a -> Identity a
resolve (LocalDefinition a) = pure a
resolve (UniqueDefinition a _) = pure a

-- just unit tests for now, ignore possibility of failure
runTests :: Module ResolvedDep ann -> [(TestName, Bool)]
runTests (Module {moTests, moExpressions}) =
  fmap runTest moTests
    where
      runTest (UnitTest testName ident) =
        case M.lookup ident moExpressions of
          Just tle ->
            case doInterpret (void $ tleExpr tle) of
              EPrim _ (PBool b) -> (testName, b)
              _ -> error "Expected a boolean result"
          Nothing ->  (testName, False)
