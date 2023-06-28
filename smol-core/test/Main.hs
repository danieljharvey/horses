module Main (main) where

import Test.Hspec
import qualified Test.Interpreter.InterpreterSpec
import qualified Test.Modules.ModulesSpec
import qualified Test.ParserSpec
import qualified Test.Typecheck.ExhaustivenessSpec
import qualified Test.Typecheck.NestingMonadSpec
import qualified Test.Typecheck.SubtypeSpec
import qualified Test.TypecheckSpec
import qualified Test.Typecheck.PatternSpec

main :: IO ()
main = hspec $ do
  Test.TypecheckSpec.spec
  Test.Typecheck.SubtypeSpec.spec
  Test.Typecheck.NestingMonadSpec.spec
  Test.Typecheck.ExhaustivenessSpec.spec
  Test.Typecheck.PatternSpec.spec
  Test.ParserSpec.spec
  Test.Interpreter.InterpreterSpec.spec
  Test.Modules.ModulesSpec.spec
