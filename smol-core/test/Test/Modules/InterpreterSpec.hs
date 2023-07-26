{-# LANGUAGE OverloadedStrings #-}

module Test.Modules.InterpreterSpec (spec) where

import Control.Monad (void)
import Data.Foldable (traverse_)
import Data.Text (Text)
import Smol.Core
import Smol.Core.Modules.Check
import Smol.Core.Modules.Interpret
import Smol.Core.Modules.Types.ModuleError
import Smol.Core.Typecheck.FromParsedExpr
import Test.Helpers
import Test.Hspec

testInterpret ::
  Text ->
  Either
    (ModuleError Annotation)
    (Expr ResolvedDep ())
testInterpret input =
  case parseModuleAndFormatError input of
    Right moduleParts -> do
      goodModule <- checkModule input moduleParts
      fmap void (interpretModule "main" (fmap getTypeAnnotation goodModule))
    Left e -> error (show e)

spec :: Spec
spec = do
  describe "Module InterpreterSpec" $ do
    describe "interpret" $ do
      let cases =
            [ (["def main = 1 + 1"], "2"),
              ( [ "def id a = a",
                  "def main = id -10"
                ],
                "-10"
              ),
              (["def id a = a", "def useId a = id a", "def main = useId 100"], "100")
            ]
      traverse_
        ( \(parts, expect) ->
            let input = joinText parts
             in it (show input <> " = " <> show expect) $ do
                  let expected :: Expr ResolvedDep ()
                      expected = void (fromParsedExpr (unsafeParseExpr expect))
                  testInterpret input
                    `shouldBe` Right expected
        )
        cases
