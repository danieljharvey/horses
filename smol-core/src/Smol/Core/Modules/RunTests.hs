{-# LANGUAGE NamedFieldPuns #-}

module Smol.Core.Modules.RunTests (runTests) where

import Smol.Core
import Smol.Core.Modules.Interpret
import Smol.Core.Modules.Types

-- just unit tests for now, ignore possibility of failure
runTests :: (Show ann, Eq ann) => Module ResolvedDep ann -> [(TestName, Bool)]
runTests wholeModule@(Module {moTests}) =
  fmap runTest moTests
  where
    runTest (UnitTest testName _) =
      case interpretModule (DITest testName) wholeModule of
        Right (EPrim _ (PBool b)) -> (testName, b)
        other -> error $ "Expected a boolean result, got " <> show other
