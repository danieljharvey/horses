module Main (main) where

import Test.Hspec
import qualified Test.IR.CompileSpec
import qualified Test.IR.DataTypesSpec
import qualified Test.IR.FromExprSpec
import qualified Test.IR.IRSpec
import qualified Test.IR.PatternSpec
import qualified Test.Interpreter.InterpreterSpec
import qualified Test.ParserSpec
import qualified Test.Transform.FlattenPatternSpec
import qualified Test.Typecheck.ExhaustivenessSpec
import qualified Test.Typecheck.NestingMonadSpec
import qualified Test.Typecheck.SubtypeSpec
import qualified Test.TypecheckSpec

main :: IO ()
main = hspec $ do
  Test.IR.DataTypesSpec.spec
  Test.IR.PatternSpec.spec
  Test.TypecheckSpec.spec
  Test.Typecheck.SubtypeSpec.spec
  Test.Typecheck.NestingMonadSpec.spec
  Test.Typecheck.ExhaustivenessSpec.spec
  Test.ParserSpec.spec
  Test.IR.FromExprSpec.spec
  Test.IR.CompileSpec.spec
  Test.IR.IRSpec.spec
  Test.Interpreter.InterpreterSpec.spec
  Test.Transform.FlattenPatternSpec.spec
