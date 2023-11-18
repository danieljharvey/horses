module Main (main) where

import Test.Hspec
import qualified Test.Modules.CheckSpec
import qualified Test.Modules.FromPartsSpec
import qualified Test.Modules.InterpreterSpec
import qualified Test.Modules.ParserSpec
import qualified Test.Modules.PrettyPrintSpec
import qualified Test.Modules.ResolveDepsSpec
import qualified Test.Modules.RunTestsSpec
import qualified Test.Modules.TypecheckSpec
import qualified Test.Typecheck.ToDictionaryPassingSpec

main :: IO ()
main = hspec $ parallel $ do
  Test.Typecheck.ToDictionaryPassingSpec.spec
  Test.Modules.CheckSpec.spec
  Test.Modules.FromPartsSpec.spec
  Test.Modules.InterpreterSpec.spec
  Test.Modules.PrettyPrintSpec.spec
  Test.Modules.ResolveDepsSpec.spec
  Test.Modules.RunTestsSpec.spec
  Test.Modules.TypecheckSpec.spec
  Test.Modules.ParserSpec.spec
