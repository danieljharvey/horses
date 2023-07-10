module Smol.Repl.Helpers.ShowTestResults (testsAllPass, printTestResults) where

import Data.Foldable (traverse_)
import Data.Monoid
import Smol.Core.Modules.Types

testsAllPass :: [(a, Bool)] -> Bool
testsAllPass = getAll . foldMap (All . snd)

printTestResults :: [(TestName, Bool)] -> IO ()
printTestResults =
  traverse_ printResult
  where
    printResult (name, True) =
      putStrLn $ "✅ " <> show name
    printResult (name, False) =
      putStrLn $ "❌ " <> show name
