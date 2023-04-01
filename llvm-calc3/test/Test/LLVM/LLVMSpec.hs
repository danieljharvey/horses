{-# LANGUAGE OverloadedStrings #-}

module Test.LLVM.LLVMSpec (spec) where

import qualified Calc.Compile.RunLLVM as Run
import Calc.Compile.ToLLVM
import Calc.Parser
import Calc.Typecheck.Elaborate
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified LLVM.AST as LLVM
import Test.Hspec

-- run the code, get the output, die
run :: LLVM.Module -> IO Text
run = fmap Run.rrResult . Run.run

testCompileIR :: (Text, Text) -> Spec
testCompileIR (input, result) = it (show input) $ do
  case parseModuleAndFormatError input of
    Left e -> error (show e)
    Right module' ->
      case elaborateModule module' of
        Left e -> error (show e)
        Right typedExpr -> do
          resp <- run (moduleToLLVM typedExpr)
          resp `shouldBe` result

spec :: Spec
spec = do
  describe "LLVMSpec" $ do
    let testExprs =
          [ ("42", "42"),
            ("(1 + 1)", "2"),
            ("1 + 2 + 3 + 4 + 5 + 6", "21"),
            ("6 * 6", "36"),
            ("100 - 1", "99"),
            ("if False then 1 else 2", "2"),
            ("if 1 == 1 then 7 else 10", "7"),
            ("if 2 == 1 then True else False", "False"),
            ("function one() { 1 } function two() { 2 } one() + two()", "3"),
            ("function increment(a: Integer) { a + 1 } increment(41)", "42"),
            ("function sum(a: Integer, b: Integer) { a + b } sum(20,22)", "42"),
            ("function inc(a: Integer) { a + 1 } inc(inc(inc(inc(0))))", "4")
          ]

    describe "From modules" $ do
      traverse_ testCompileIR testExprs
