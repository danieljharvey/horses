module Main (main) where

import Test.Hspec
import qualified Test.ParserSpec
import qualified Test.TransformSpec
import qualified Test.Typecheck.ExhaustivenessSpec
import qualified Test.Typecheck.NestingMonadSpec
import qualified Test.Typecheck.PatternSpec
import qualified Test.Typecheck.SubtypeSpec
import qualified Test.Typecheck.TypeclassSpec
import qualified Test.TypecheckSpec

main :: IO ()
main = hspec $ parallel $ do
  Test.TypecheckSpec.spec
  Test.Typecheck.SubtypeSpec.spec
  Test.Typecheck.NestingMonadSpec.spec
  Test.Typecheck.ExhaustivenessSpec.spec
  Test.Typecheck.PatternSpec.spec
  Test.Typecheck.TypeclassSpec.spec
  Test.ParserSpec.spec
  Test.TransformSpec.spec
