module Main (main) where

import Test.Hspec
import qualified Test.Interpreter.InterpreterSpec
import qualified Test.Modules.FromPartsSpec
import qualified Test.Modules.InterpreterSpec
import qualified Test.Modules.ResolveDepsSpec
import qualified Test.Modules.RunTestsSpec
import qualified Test.Modules.TypecheckSpec
import qualified Test.ParserSpec
import qualified Test.Typecheck.ExhaustivenessSpec
import qualified Test.Typecheck.NestingMonadSpec
import qualified Test.Typecheck.PatternSpec
import qualified Test.Typecheck.SubtypeSpec
import qualified Test.TypecheckSpec

main :: IO ()
main = hspec $ do
  Test.TypecheckSpec.spec
  Test.Typecheck.SubtypeSpec.spec
  Test.Typecheck.NestingMonadSpec.spec
  Test.Typecheck.ExhaustivenessSpec.spec
  Test.Typecheck.PatternSpec.spec
  Test.ParserSpec.spec
  Test.Interpreter.InterpreterSpec.spec
  Test.Modules.FromPartsSpec.spec
  Test.Modules.InterpreterSpec.spec
  Test.Modules.ResolveDepsSpec.spec
  Test.Modules.RunTestsSpec.spec
  Test.Modules.TypecheckSpec.spec
