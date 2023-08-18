{-# LANGUAGE OverloadedStrings #-}

module Test.Modules.InterpreterSpec (spec) where

import Control.Monad (void)
import Data.Foldable (traverse_)
import Data.Text (Text)
import Error.Diagnose (defaultStyle, printDiagnostic, stdout)
import Smol.Core
import Smol.Core.Modules.Check
import Smol.Core.Modules.Interpret
import Smol.Core.Modules.Types.ModuleError
import Smol.Core.Typecheck.FromParsedExpr
import Test.Helpers
import Test.Hspec

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
              ( [ "def useEquals : Int -> Bool",
                  "def useEquals a = equals a (1: Int)",
                  "def main : Bool",
                  "def main = useEquals 2"
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
              ( ["def main = equals ((1:Int), (2: Int)) ((1: Int), (2: Int))"],
                "True"
              ),
              ( [ "def main : Bool",
                  "def main = useEquals (1: Int) (2: Int)",
                  "def useEquals : (Eq a) => a -> a -> Bool",
                  "def useEquals a b = equals a b"
                ],
                "False"
              ),
              ( [ "def main : Bool",
                  "def main = notEquals (1: Int) (2: Int)",
                  "def notEquals : (Eq a) => a -> a -> Bool",
                  "def notEquals a b = if isEquals a b then False else True",
                  "def isEquals : (Eq a) => a -> a -> Bool",
                  "def isEquals a b = equals a b"
                ],
                "True"
              ),
              ( [ "instance Eq String = \\a -> \\b -> a == b",
                  "def main : Bool",
                  "def main = equals (\"cat\" : String) (\"cat\" : String)"
                ],
                "True"
              )
              {-
                  -- next we need to work out the dependencies of our typeclass
                  -- functions
              ( ["type Pet = Dog | Cat | Rat",
                  "instance Eq Pet = \\a -> \\b -> case (a,b) of (Dog,Dog) -> True | (Cat, Cat) -> True | (Rat,Rat) -> True | _ -> False",
                  "def main : Bool",
                  "def main = equals Dog Cat"],
                  "False"

              ) -}
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
