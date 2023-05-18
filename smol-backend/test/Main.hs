module Main (main) where

import Test.Hspec
import qualified Test.IR.CompileSpec
import qualified Test.IR.DataTypesSpec
import qualified Test.IR.FromExprSpec
import qualified Test.IR.IRSpec
import qualified Test.IR.PatternSpec

main :: IO ()
main = hspec $ do
  Test.IR.DataTypesSpec.spec
  Test.IR.PatternSpec.spec
  Test.IR.FromExprSpec.spec
  Test.IR.CompileSpec.spec
  Test.IR.IRSpec.spec
