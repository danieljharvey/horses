module Main (main) where

import Test.Hspec
import qualified Test.Interpreter.InterpreterSpec

main :: IO ()
main = hspec $ parallel $ do
  Test.Interpreter.InterpreterSpec.spec
