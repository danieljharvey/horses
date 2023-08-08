{-# LANGUAGE OverloadedStrings #-}

module Test.Modules.InterpreterSpec (spec) where
import Error.Diagnose (defaultStyle, printDiagnostic, stdout)
import Smol.Core.Helpers
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
import Smol.Core.Modules.Types

showModuleError :: ModuleError Annotation -> IO () 
showModuleError modErr = 
  printDiagnostic stdout True True 2 defaultStyle (moduleErrorDiagnostic modErr)

testInterpret ::
  Text ->
  Either
    (ModuleError Annotation)
    (Expr ResolvedDep ())
testInterpret input =
  case parseModuleAndFormatError input of
    Right moduleParts -> do
      goodModule <- checkModule input moduleParts
      tracePrettyM "checked module" (getExprAnnotation . tleExpr <$> moExpressions goodModule)
      fmap void (interpretModule "main" (fmap getTypeAnnotation goodModule))
    Left e -> error (show e)

spec :: Spec
spec = do
  describe "Module InterpreterSpec" $ do
    fdescribe "interpret" $ do
      let cases =
            [ (["def main = 1 + 1"], "2"),
              ( [ "def id a = a",
                  "def main = id -10"
                ],
                "-10"
              ),
              ( [ "def id a = a",
                  "def useId a = id a",
                  "def main = useId 100"
                ],
                "100"
              ),
              ( [ "def main = equals (1: Int) (2: Int)"
                ],
                "False"
              ),
              ( [ "def useEquals = equals (1: Int) (2: Int)",
                  "def main = useEquals"
                ],
                "False"
              ),
              ( [ "def useEquals : Bool -> Bool",
                  "def useEquals a = equals (2: Int) (1: Int)",
                  "def main : Bool",
                  "def main = useEquals True"
                ],
                "False"
              ),
              (
                ["def uselessConstraint : (Eq a) => Int -> Int",
                "def uselessConstraint a = a + 1",
                "def main = uselessConstraint 100"],"101")
            ]
      traverse_
        ( \(parts, expect) ->
            let input = joinText parts
             in it (show input <> " = " <> show expect) $ do
                  let expected :: Expr ResolvedDep ()
                      expected = void (fromParsedExpr (unsafeParseExpr expect))
                  
                  let result = testInterpret input
                  case result of
                    Left e -> showModuleError e
                    _ -> pure ()

                  result `shouldBe` Right expected
        )
        cases
