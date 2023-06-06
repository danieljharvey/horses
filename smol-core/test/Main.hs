module Main (main) where

import qualified Test.EliminateGlobalsSpec
import Test.Hspec
import qualified Test.Interpreter.InterpreterSpec
import qualified Test.Modules.ModulesSpec
import qualified Test.ParserSpec
import qualified Test.Typecheck.ExhaustivenessSpec
import qualified Test.Typecheck.NestingMonadSpec
import qualified Test.Typecheck.SubtypeSpec
import qualified Test.TypecheckSpec

main :: IO ()
main = hspec $ do
  Test.TypecheckSpec.spec
  Test.Typecheck.SubtypeSpec.spec
  Test.Typecheck.NestingMonadSpec.spec
  Test.Typecheck.ExhaustivenessSpec.spec
  Test.ParserSpec.spec
  Test.Interpreter.InterpreterSpec.spec
  Test.Modules.ModulesSpec.spec
  Test.EliminateGlobalsSpec.spec
