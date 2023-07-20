{-# LANGUAGE NamedFieldPuns #-}

module Smol.Core.Modules.RunTests (runTests) where

import Data.Functor (void)
import qualified Data.Map.Strict as M
import Smol.Core
import Smol.Core.Interpreter.Types.InterpreterError
import Smol.Core.Interpreter.Types.Stack
import Smol.Core.Modules.Types

doInterpret :: Expr ResolvedDep () -> Either (InterpreterError ()) (Expr ResolvedDep ())
doInterpret =
  (fmap . fmap) edAnnotation
    . interpret mempty
    . addEmptyStackFrames

-- just unit tests for now, ignore possibility of failure
runTests :: Module ResolvedDep ann -> [(TestName, Bool)]
runTests (Module {moTests, moExpressions}) =
  fmap runTest moTests
  where
    runTest (UnitTest testName ident) =
      case M.lookup ident moExpressions of
        Just tle ->
          case doInterpret (void $ tleExpr tle) of
            Right (EPrim _ (PBool b)) -> (testName, b)
            _ -> error "Expected a boolean result"
        Nothing -> (testName, False)
