{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Modules.Check
  ( spec,
  )
where

import Control.Monad.IO.Class
import Data.Either (isLeft, isRight)
import Data.Functor
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Language.Mimsa.Actions.Modules.Check as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Core
import Language.Mimsa.Modules.Check
import Language.Mimsa.Modules.FromParts
import Language.Mimsa.Types.Error
import Test.Data.Prelude
import Test.Hspec
import Test.Utils.Helpers

modulesPath :: FilePath
modulesPath = "compiler/test/modules/"

exprAndTypeFromParts' ::
  (Monoid ann) =>
  [DefPart ann] ->
  Expr Name ann ->
  Either (Error Annotation) (Expr Name ann)
exprAndTypeFromParts' =
  exprAndTypeFromParts (DIName "test")

testModules :: Map ModuleHash (Module Annotation)
testModules = M.singleton preludeHash prelude

checkModule' :: Text -> Either (Error Annotation) (Module ())
checkModule' t = do
  let action = do
        (a, _) <- Actions.checkModule testModules t
        (b, _) <- Actions.checkModule testModules (prettyPrint a)
        if (a $> ()) /= (b $> ())
          then
            error $
              "Does not match!\n\n"
                <> show a
                <> "\n\n"
                <> show b
                <> "\n\nWhen re-parsing\n\n"
                <> show (prettyPrint a)
          else
            let tyA = getModuleType a
                tyB = getModuleType b
             in if (tyA $> ()) == (tyB $> ())
                  then pure (a $> mempty)
                  else
                    error $
                      "Types are different:\n\n"
                        <> T.unpack (prettyPrint tyA)
                        <> "\n\n"
                        <> T.unpack (prettyPrint tyB)
  getResult <$> Actions.run mempty action

getResult :: (a, b, c) -> c
getResult (_, _, c) = c

checkModuleType :: Text -> Either (Error Annotation) (Module (Type Annotation))
checkModuleType t =
  fst . getResult
    <$> Actions.run
      mempty
      ( Actions.checkModule testModules t
      )

spec :: Spec
spec = do
  describe "modules" $ do
    describe "CheckModule" $ do
      it "1 parses correctly" $ do
        let filePath = modulesPath <> "1.mimsa"
        fileContents <- liftIO $ T.readFile filePath
        checkModule' fileContents `shouldSatisfy` isRight
      it "2 errors because duplicate definitions" $ do
        let filePath = modulesPath <> "2.mimsa"
        fileContents <- liftIO $ T.readFile filePath
        checkModule' fileContents
          `shouldBe` Left (ModuleErr (DuplicateDefinition (DIName "duplicate")))
      it "3 errors because duplicate type name" $ do
        let filePath = modulesPath <> "3.mimsa"
        fileContents <- liftIO $ T.readFile filePath
        checkModule' fileContents
          `shouldBe` Left (ModuleErr (DuplicateTypeName "Maybe"))
      -- to implement
      it "4 errors because duplicate constructor" $ do
        let filePath = modulesPath <> "4.mimsa"
        fileContents <- liftIO $ T.readFile filePath
        checkModule' fileContents
          `shouldBe` Left (ModuleErr (DuplicateConstructor "Nothing"))
      it "5 errors because it refers to an non-existent value" $ do
        let filePath = modulesPath <> "5.mimsa"
        fileContents <- liftIO $ T.readFile filePath
        checkModule' fileContents
          `shouldBe` Left (ModuleErr (CannotFindValues (S.singleton (DIName "eatEgg"))))
      it "6 errors because it doesn't typecheck" $ do
        let filePath = modulesPath <> "6.mimsa"
        fileContents <- liftIO $ T.readFile filePath
        checkModule' fileContents
          `shouldSatisfy` \case
            Left (ModuleErr (DefDoesNotTypeCheck _ (DIName "doesntTypecheck") _)) -> True
            _ -> False
      it "7 errors because annotation means it doesn't typecheck" $ do
        let filePath = modulesPath <> "7.mimsa"
        fileContents <- liftIO $ T.readFile filePath
        checkModule' fileContents
          `shouldSatisfy` \case
            Left (ModuleErr (DefDoesNotTypeCheck _ (DIName "doesntTypecheckBecauseAnnotation") _)) -> True
            _ -> False
      it "8 is broken because of partial type annotation" $ do
        let filePath = modulesPath <> "8.mimsa"
        fileContents <- liftIO $ T.readFile filePath
        checkModule' fileContents
          `shouldSatisfy` isLeft
      it "9 fails to typecheck because we cannot have polymorphic id function with a set input type" $ do
        let filePath = modulesPath <> "9.mimsa"
        fileContents <- liftIO $ T.readFile filePath
        checkModule' fileContents
          `shouldSatisfy` isLeft
      it "10 contains an infix function and typechecks" $ do
        let filePath = modulesPath <> "10.mimsa"
        fileContents <- liftIO $ T.readFile filePath
        checkModule' fileContents
          `shouldSatisfy` isRight

    describe "exprAndTypeFromParts" $ do
      it "No args" $ do
        let expr = unsafeParseExpr "100"
        exprAndTypeFromParts' mempty expr `shouldBe` Right expr
      it "Single non-typed arg" $ do
        let expr = unsafeParseExpr "100"
            parts = [DefArg (Identifier () "a")]
        exprAndTypeFromParts' parts expr
          `shouldBe` Right (MyLambda mempty (Identifier mempty "a") expr)

      it "Two non-typed args" $ do
        let expr = unsafeParseExpr "100"
            parts = [DefArg (Identifier () "a"), DefArg (Identifier () "b")]
        exprAndTypeFromParts' parts expr
          `shouldBe` Right
            ( MyLambda
                mempty
                (Identifier mempty "a")
                (MyLambda mempty (Identifier mempty "b") expr)
            )
      it "Typed arg and return type" $ do
        let expr = unsafeParseExpr "True"
            parts =
              [ DefTypedArg (Identifier () "str") mtString,
                DefTypedArg (Identifier () "int") mtInt,
                DefType mtBool
              ]
        exprAndTypeFromParts' parts expr
          `shouldBe` Right
            ( MyAnnotation
                mempty
                (mtFun mtString (mtFun mtInt mtBool))
                ( MyLambda
                    mempty
                    (Identifier mempty "str")
                    (MyLambda mempty (Identifier mempty "int") expr)
                )
            )

      it "Errors on typed arg but no return type" $ do
        let expr = unsafeParseExpr "True"
            parts =
              [ DefTypedArg (Identifier () "str") mtString,
                DefTypedArg (Identifier () "int") mtInt
              ]
        exprAndTypeFromParts' parts expr
          `shouldSatisfy` isLeft

      describe "Examples" $ do
        it "Empty file" $
          checkModule' "" `shouldBe` Right mempty
        describe "definitions" $ do
          it "Single constant" $
            let expectedExpr = unsafeParseExpr "100" $> mempty
                exprs = M.singleton (DIName "noSig") expectedExpr
                expectedModule = mempty {moExpressions = exprs}
             in checkModule' "def noSig = 100"
                  `shouldBe` Right expectedModule
          it "Two constants" $
            let exprs =
                  M.fromList
                    [ (DIName "one", unsafeParseExpr "1" $> mempty),
                      (DIName "two", unsafeParseExpr "2" $> mempty)
                    ]
                expectedModule = mempty {moExpressions = exprs}
             in checkModule' "def one = 1\ndef two = 2"
                  `shouldBe` Right expectedModule
          it "id Function" $
            let exprs =
                  M.fromList
                    [ (DIName "id", unsafeParseExpr "\\a -> a" $> mempty)
                    ]
                expectedModule = mempty {moExpressions = exprs}
             in checkModule' "def id a = a"
                  `shouldBe` Right expectedModule
          it "const Function" $
            let exprs =
                  M.fromList
                    [ (DIName "const", unsafeParseExpr "\\a -> \\b -> a" $> mempty)
                    ]
                expectedModule = mempty {moExpressions = exprs}
             in checkModule' "def const a b = a"
                  `shouldBe` Right expectedModule
          it "multiple Functions" $
            let exprs =
                  M.fromList
                    [ (DIName "id", unsafeParseExpr "\\a -> a" $> mempty),
                      (DIName "const", unsafeParseExpr "\\a -> \\b -> a" $> mempty)
                    ]
                expectedModule = mempty {moExpressions = exprs}
             in checkModule' "def id a = a\ndef const a b = a"
                  `shouldBe` Right expectedModule
      describe "datatype definitions" $ do
        it "single datatype definition" $
          let dts =
                M.fromList
                  [ ( "Maybe",
                      DataType
                        "Maybe"
                        ["a"]
                        ( M.fromList
                            [ ("Just", [mtVar "a"]),
                              ("Nothing", mempty)
                            ]
                        )
                    )
                  ]
              expectedModule = mempty {moDataTypes = dts}
           in checkModule' "type Maybe a = Just a | Nothing"
                `shouldBe` Right expectedModule
        it "single datatype definition with definition after" $
          let dts =
                M.fromList
                  [ ( "Maybe",
                      DataType
                        "Maybe"
                        ["a"]
                        ( M.fromList
                            [ ("Just", [mtVar "a"]),
                              ("Nothing", mempty)
                            ]
                        )
                    )
                  ]
              exprs = M.fromList [(DIName "a", unsafeParseExpr "1" $> mempty)]
              expectedModule = mempty {moDataTypes = dts, moExpressions = exprs}
           in checkModule' "type Maybe a = Just a | Nothing\ndef a = 1"
                `shouldBe` Right expectedModule
      describe "exports" $ do
        it "export id function" $ do
          let exprs =
                M.fromList
                  [(DIName "id", unsafeParseExpr "\\a -> a" $> mempty)]
              exports = S.singleton (DIName "id")
              expectedModule = mempty {moExpressions = exprs, moExpressionExports = exports}
          checkModule' "export def id a = a"
            `shouldBe` Right expectedModule

      describe "definitions with types" $ do
        it "function with full signature" $
          let exprs =
                M.fromList
                  [ ( DIName "const",
                      MyAnnotation
                        mempty
                        (unsafeParseMonoType "String -> Int -> String" $> mempty)
                        (unsafeParseExpr "\\a -> \\b -> a" $> mempty)
                    )
                  ]
              expectedModule = mempty {moExpressions = exprs}
           in checkModule' "def const (a: String) (b: Int) : String = a"
                `shouldBe` Right expectedModule
        it "function with signature where not all args have names" $
          let exprs =
                M.fromList
                  [ ( DIName "returnFunc",
                      MyAnnotation
                        mempty
                        (unsafeParseMonoType "String -> Int -> String" $> mempty)
                        (unsafeParseExpr "\\a -> \\b -> a" $> mempty)
                    )
                  ]
              expectedModule = mempty {moExpressions = exprs}
           in checkModule' "def returnFunc (a: String) : Int -> String = \\b -> a"
                `shouldBe` Right expectedModule

        it "function where signature has partial types" $
          checkModule' "def const (a: String) b : a = a"
            `shouldBe` Left (ModuleErr (DefMissingTypeAnnotation (DIName "const") "b"))

        it "function where signature has incomplete type annotations explodes" $
          checkModule' "def const (a: String) b = a"
            `shouldBe` Left (ModuleErr (DefMissingReturnType (DIName "const")))

        it "multiple functions with signatures" $
          let exprs =
                M.fromList
                  [ ( DIName "fmap",
                      MyAnnotation
                        mempty
                        (unsafeParseMonoType "(a -> b) -> Maybe a -> Maybe b" $> mempty)
                        ( unsafeParseExpr "\\f -> \\maybeA -> match maybeA with Just a -> Just (f a) | Nothing -> Nothing" $> mempty
                        )
                    ),
                    ( DIName "inc",
                      unsafeParseExpr "\\a -> a + 1" $> mempty
                    )
                  ]
              dts =
                M.fromList
                  [ ( "Maybe",
                      DataType
                        "Maybe"
                        ["a"]
                        ( M.fromList
                            [ ("Just", [mtVar "a"]),
                              ("Nothing", mempty)
                            ]
                        )
                    )
                  ]

              expectedModule =
                mempty
                  { moExpressions = exprs,
                    moDataTypes = dts
                  }
           in checkModule' "type Maybe a = Just a | Nothing\ndef fmap (f: a -> b) (maybeA: Maybe a): Maybe b = match maybeA with Just a -> Just (f a) | Nothing -> Nothing\n\n\ndef inc a = a + 1"
                `shouldBe` Right expectedModule

      describe "check types" $ do
        it "broken type declaration" $
          checkModuleType
            ( joinLines
                ["type Maybe a = Just b | Nothing"]
            )
            `shouldSatisfy` isLeft

        it "one type uses another correctly" $
          checkModuleType
            ( joinLines
                [ "type Maybe a = Just a | Nothing",
                  "type Parser a = Parser (String -> Maybe (String, a))"
                ]
            )
            `shouldSatisfy` isRight

        -- need to implement kind checking in datatype declarations
        xit "one type uses another incorrectly and fails" $
          checkModuleType
            ( joinLines
                [ "type Maybe a = Just a | Nothing",
                  "type Parser a = Parser (String -> Maybe Int (String, a))"
                ]
            )
            `shouldSatisfy` isLeft

      describe "tests" $ do
        it "Accepts a trivial test" $
          checkModuleType
            (joinLines ["test \"2 equals 2\" = 2 == 2"])
            `shouldSatisfy` isRight

        it "Does not accept a test with an empty name" $
          checkModuleType
            (joinLines ["test \"\" = 2 == 2"])
            `shouldSatisfy` isLeft

        it "Does not accept a duplicated test name" $
          checkModuleType
            ( joinLines
                [ "test \"test\" = 2 == 2",
                  "test \"test\" = 2 + 2 == 4"
                ]
            )
            `shouldSatisfy` isLeft

        it "Accepts a trivial test that refers to another expression" $
          checkModuleType
            ( joinLines
                [ "test \"id 2 equals 2\" = id 2 == 2",
                  "def id a = a"
                ]
            )
            `shouldSatisfy` isRight

      describe "imports" $ do
        it "uses fst from Prelude" $
          getModuleType
            <$> checkModuleType
              ( joinLines
                  [ "import * from " <> prettyPrint preludeHash,
                    "export def useFst = fst (1,2)"
                  ]
              )
            `shouldSatisfy` isRight
        it "uses fst from Prelude but it shouldn't typecheck" $
          checkModuleType
            ( joinLines
                [ "import * from " <> prettyPrint preludeHash,
                  "def useFst = fst True"
                ]
            )
            `shouldSatisfy` isLeft
        it "errors when locally defining fst" $
          checkModuleType
            ( joinLines
                [ "import * from " <> prettyPrint preludeHash,
                  "def fst pair = let (a,_) = pair in a"
                ]
            )
            `shouldBe` Left (ModuleErr $ DefinitionConflictsWithImport (DIName "fst") preludeHash)
        it "uses Either from Prelude" $
          checkModuleType
            ( joinLines
                [ "import * from " <> prettyPrint preludeHash,
                  "def withEither val = match val with Right a -> [a,a] | Left _ -> []"
                ]
            )
            `shouldSatisfy` isRight

        it "uses Either and <|> from Prelude" $
          checkModuleType
            ( joinLines
                [ "import * from " <> prettyPrint preludeHash,
                  "def nice = Left 1 <|> Right True"
                ]
            )
            `shouldSatisfy` isRight
        it "errors when locally defining Either" $
          checkModuleType
            ( joinLines
                [ "type Either b c = Left b | Right c",
                  "import * from " <> prettyPrint preludeHash
                ]
            )
            `shouldBe` Left (ModuleErr $ DuplicateTypeName "Either")

        it "Errors when adding a duplicate Right constructor" $
          checkModule'
            ( joinLines
                [ "type Result e a = Failure e | Right a",
                  "type Either e a = Left e | Right a"
                ]
            )
            `shouldSatisfy` isLeft

        it "Imports parse and pretty print" $
          checkModule'
            ( joinLines
                [ "type Maybe a = Just a | Nothing",
                  "import * from " <> prettyPrint preludeHash
                ]
            )
            `shouldSatisfy` isRight

        it "Parses namespaced import" $
          checkModuleType
            ( joinLines
                [ "import Prelude from " <> prettyPrint preludeHash
                ]
            )
            `shouldSatisfy` isRight

        it ("Uses fst from Prelude with named import: " <> T.unpack (prettyPrint preludeHash)) $
          checkModuleType
            ( joinLines
                [ "import Prelude from " <> prettyPrint preludeHash,
                  "def withFst = Prelude.fst (True, 1)"
                ]
            )
            `shouldSatisfy` isRight

        it ("Uses Right and Left from Prelude with named import: " <> T.unpack (prettyPrint preludeHash)) $
          checkModuleType
            ( joinLines
                [ "import Prelude from " <> prettyPrint preludeHash,
                  "def useEither eA = match eA with Prelude.Right a -> [a] | Prelude.Left _ -> []"
                ]
            )
            `shouldSatisfy` isRight

        it ("Uses Either type from Prelude with named import: " <> T.unpack (prettyPrint preludeHash)) $
          checkModuleType
            ( joinLines
                [ "import Prelude from " <> prettyPrint preludeHash,
                  "def useEither (eA: Prelude.Either e String): String = match eA with Prelude.Right a -> a | Prelude.Left _ -> \"\""
                ]
            )
            `shouldSatisfy` isRight

        it "Uses Either type from Prelude without specifying namespace fails" $
          checkModuleType
            ( joinLines
                [ "import Prelude from " <> prettyPrint preludeHash,
                  "def useEither (eA: Either e String): String = match eA with Right a -> a | _ -> \"\""
                ]
            )
            `shouldSatisfy` isLeft

        it "Locally defined Either does not mess with namespace imported Either" $
          checkModuleType
            ( joinLines
                [ "import Prelude from " <> prettyPrint preludeHash,
                  "type Either e a = Left e | Right a",
                  "def convert (val: Either e a): Prelude.Either e a = ",
                  "  match val with Right a -> Prelude.Right a | Left e -> Prelude.Left e"
                ]
            )
            `shouldSatisfy` isRight
