{-# LANGUAGE OverloadedStrings #-}

module Test.Modules.InterpreterSpec (spec) where

import Control.Monad (void)
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Error.Diagnose as Diag
import Smol.Core
import Smol.Core.Annotations
import Smol.Modules.Check
import Smol.Modules.Interpret
import Smol.Modules.Parser
import Smol.Modules.Types.DefIdentifier
import Smol.Modules.Types.ModuleError
import Test.Helpers
import Test.Hspec

showModuleError :: Text -> ModuleError Annotation -> IO ()
showModuleError input modErr =
  Diag.printDiagnostic
    Diag.stdout
    Diag.WithUnicode
    (Diag.TabSize 2)
    Diag.defaultStyle
    (moduleErrorDiagnostic input modErr)

testInterpret ::
  Text ->
  Either
    (ModuleError Annotation)
    (Expr ResolvedDep ())
testInterpret input =
  case parseModuleAndFormatError input of
    Right moduleParts -> do
      goodModule <- checkModule input moduleParts
      fmap void (interpretModule (DIName "main") (fmap getTypeAnnotation goodModule))
    Left e -> error (show e)

spec :: Spec
spec = do
  describe "Module InterpreterSpec" $ do
    describe "interpret" $ do
      let cases =
            [ ( ["def main: Int { 1 + 1 }"],
                "2"
              ),
              ( [ "def id (a: a): a { a }",
                  "def main : Int { id -10 }"
                ],
                "-10"
              ),
              ( [ "def id (a: a): a { a }",
                  "def useId (a: a): a { id a }",
                  "def main : Int {useId 100 }"
                ],
                "100"
              ),
              ( [ "class Eq a { equals: a -> a -> Bool }",
                  "instance Eq Int { \\a -> \\b -> a == b }",
                  "def main : Bool {equals (1: Int) (2: Int) }"
                ],
                "False"
              ),
              ( [ "class Eq a { equals: a -> a -> Bool }",
                  "instance Eq Int { \\a -> \\b -> a == b }",
                  "def useEquals : Bool { equals (1: Int) (2: Int) }",
                  "def main : Bool { useEquals }"
                ],
                "False"
              ),
              ( [ "class Eq a { equals: a -> a -> Bool }",
                  "instance Eq Int { \\a -> \\b -> a == b }",
                  "def useEquals (a: Int): Bool {equals a (1: Int) }",
                  "def main : Bool { useEquals 2 }"
                ],
                "False"
              ),
              ( [ "class Eq a { equals: a -> a -> Bool }",
                  "instance Eq Int { \\a -> \\b -> a == b }",
                  "def useEquals (a: Bool): Bool { equals (2: Int) (1: Int) }",
                  "def main : Bool { useEquals True }"
                ],
                "False"
              ),
              ( [ "class Eq a { equals: a -> a -> Bool }",
                  "instance Eq Int { \\a -> \\b -> a == b }",
                  "instance (Eq a, Eq b) => Eq (a,b) { \\a -> \\b -> case (a,b) {((a1, b1), (a2, b2)) -> if equals a1 a2 then equals b1 b2 else False } }",
                  "def main : Bool { equals ((1:Int), (2: Int)) ((1: Int), (2: Int)) }"
                ],
                "True"
              ),
              ( [ "class Eq a { equals: a -> a -> Bool }",
                  "instance Eq Int { \\a -> \\b -> a == b }",
                  "def main : Bool { useEquals (1: Int) (2: Int) }",
                  "def useEquals (Eq a) => (a: a) (b: a) : Bool { equals a b }"
                ],
                "False"
              ),
              ( [ "class Eq a { equals: a -> a -> Bool }",
                  "instance Eq Int { \\a -> \\b -> a == b }",
                  "def main : Bool { notEquals (1: Int) (2: Int) }",
                  "def notEquals (Eq a) => (a: a) (b: a): Bool { if isEquals a b then False else True }",
                  "def isEquals (Eq a) => (a: a) (b: a): Bool { equals a b }"
                ],
                "True"
              ),
              ( [ "class Eq a { equals: a -> a -> Bool }",
                  "instance Eq String { \\a -> \\b -> a == b }",
                  "def main : Bool { equals (\"cat\" : String) (\"cat\" : String) }"
                ],
                "True"
              ),
              ( [ "class Eq a { equals: a -> a -> Bool }",
                  "instance Eq Int { \\a -> \\b -> a == b }",
                  "class Semigroup a { mappend: a -> a -> a }",
                  "instance Semigroup Int { \\a -> \\b -> a + b }",
                  "def main : Bool { equals (mappend (20 : Int) (22 : Int)) (42 : Int) }"
                ],
                "True"
              ),
              ( [ "type Pet = Dog | Cat | Rat",
                  "class Eq a { equals: a -> a -> Bool }",
                  "instance Eq Pet { \\a -> \\b -> case (a,b) { (Dog, Dog) -> True, (Cat, Cat) -> True, (Rat, Rat) -> True, _ -> False } }",
                  "def main : Bool { equals Dog Rat }"
                ],
                "False"
              ),
              ( [ "class Eq a { equals: a -> a -> Bool }",
                  "instance Eq Int { \\a -> \\b -> a == b }",
                  "type Maybe a = Just a | Nothing",
                  "instance (Eq a) => Eq (Maybe a) { \\ma -> \\mb -> case (ma, mb) { (Just a, Just b) -> equals a b, (Nothing, Nothing) -> True, _ -> False } }",
                  "def main : Bool { equals (Just (1: Int)) Nothing }"
                ],
                "False"
              ),
              ( [ "type Natural = Suc Natural | Zero",
                  "class Show a { show: a -> String }",
                  "instance Show Natural { \\nat -> ",
                  "case nat { Suc n -> \"S \" + show n ",
                  ", _ -> \"Z\"} }",
                  "def main : String { show (Suc Zero) }"
                ],
                "\"S Z\""
              )
            ]
      traverse_
        ( \(parts, expect) ->
            let input = joinText parts
             in it (show input <> " = " <> show expect) $ do
                  let expected :: Expr ResolvedDep ()
                      expected = void (fromParsedExpr (unsafeParseExpr expect))

                  let result = testInterpret input
                  case result of
                    Left e -> showModuleError input e
                    _ -> pure ()

                  result `shouldBe` Right expected
        )
        cases
