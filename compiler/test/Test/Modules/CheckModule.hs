{-# LANGUAGE OverloadedStrings #-}

module Test.Modules.CheckModule
  ( spec,
  )
where

import Control.Monad.IO.Class
import Data.Either
import Data.Functor
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as T
import Language.Mimsa.Modules.Check
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Test.Hspec
import Test.Utils.Helpers

modulesPath :: FilePath
modulesPath = "test/modules/"

checkModule' :: Text -> Either (Error Annotation) (Module ())
checkModule' t = do
  a <- checkModule t
  pure (a $> mempty)

spec :: Spec
spec = do
  describe "CheckModule" $ do
    it "1 parses correctly" $ do
      let filePath = modulesPath <> "1.mimsa"
      fileContents <- liftIO $ T.readFile filePath
      checkModule' fileContents `shouldSatisfy` isRight
    it "2 errors because duplicate definitions" $ do
      let filePath = modulesPath <> "2.mimsa"
      fileContents <- liftIO $ T.readFile filePath
      checkModule' fileContents
        `shouldBe` Left (ModuleErr (DuplicateDefinition "duplicate"))
    it "3 errors because duplicate type name" $ do
      let filePath = modulesPath <> "3.mimsa"
      fileContents <- liftIO $ T.readFile filePath
      checkModule' fileContents
        `shouldBe` Left (ModuleErr (DuplicateTypeName "Maybe"))
    -- to implement
    xit "4 errors because duplicate constructor" $ do
      let filePath = modulesPath <> "4.mimsa"
      fileContents <- liftIO $ T.readFile filePath
      checkModule' fileContents
        `shouldBe` Left (ModuleErr (DuplicateConstructor "Nothing"))
    -- to implement
    xit "5 errors because it refers to an non-existent value" $ do
      let filePath = modulesPath <> "5.mimsa"
      fileContents <- liftIO $ T.readFile filePath
      checkModule' fileContents
        `shouldBe` Left (ModuleErr (CannotFindValue "eatEgg"))

  describe "exprAndTypeFromParts" $ do
    it "No args" $ do
      let expr = unsafeParseExpr "100"
      exprAndTypeFromParts mempty expr `shouldBe` (expr, Nothing)
    it "Single non-typed arg" $ do
      let expr = unsafeParseExpr "100"
          parts = [DefArg (Identifier () "a")]
      exprAndTypeFromParts parts expr
        `shouldBe` ( MyLambda mempty (Identifier mempty "a") expr,
                     Nothing
                   )
    it "Two non-typed args" $ do
      let expr = unsafeParseExpr "100"
          parts = [DefArg (Identifier () "a"), DefArg (Identifier () "b")]
      exprAndTypeFromParts parts expr
        `shouldBe` ( MyLambda
                       mempty
                       (Identifier mempty "a")
                       (MyLambda mempty (Identifier mempty "b") expr),
                     Nothing
                   )
    it "Typed arg and return type" $ do
      let expr = unsafeParseExpr "True"
          parts =
            [ DefTypedArg (Identifier () "str") mtString,
              DefTypedArg (Identifier () "int") mtInt,
              DefType mtBool
            ]
      exprAndTypeFromParts parts expr
        `shouldBe` ( MyLambda
                       mempty
                       (Identifier mempty "str")
                       (MyLambda mempty (Identifier mempty "int") expr),
                     Just (mtFun mtString (mtFun mtInt mtBool))
                   )
    it "Typed arg with no return type" $ do
      let expr = unsafeParseExpr "True"
          parts =
            [ DefTypedArg (Identifier () "str") mtString,
              DefTypedArg (Identifier () "int") mtInt
            ]
      exprAndTypeFromParts parts expr
        `shouldBe` ( MyLambda
                       mempty
                       (Identifier mempty "str")
                       (MyLambda mempty (Identifier mempty "int") expr),
                     Just (mtFun mtString (mtFun mtInt (mtVar "returnType")))
                   )

    describe "Examples" $ do
      it "Empty file" $
        checkModule' "" `shouldBe` Right mempty
      describe "definitions" $ do
        it "Single constant" $
          let expectedExpr = unsafeParseExpr "100" $> mempty
              exprs = M.singleton "noSig" (Nothing, expectedExpr)
              expectedModule = mempty {moExpressions = exprs}
           in checkModule' "def noSig = 100"
                `shouldBe` Right expectedModule
        it "Two constants" $
          let exprs =
                M.fromList
                  [ ("one", (Nothing, unsafeParseExpr "1" $> mempty)),
                    ("two", (Nothing, unsafeParseExpr "2" $> mempty))
                  ]
              expectedModule = mempty {moExpressions = exprs}
           in checkModule' "def one = 1\ndef two = 2"
                `shouldBe` Right expectedModule
        it "id Function" $
          let exprs =
                M.fromList
                  [ ("id", (Nothing, unsafeParseExpr "\\a -> a" $> mempty))
                  ]
              expectedModule = mempty {moExpressions = exprs}
           in checkModule' "def id a = a"
                `shouldBe` Right expectedModule
        it "const Function" $
          let exprs =
                M.fromList
                  [ ("const", (Nothing, unsafeParseExpr "\\a -> \\b -> a" $> mempty))
                  ]
              expectedModule = mempty {moExpressions = exprs}
           in checkModule' "def const a b = a"
                `shouldBe` Right expectedModule
        it "multiple Functions" $
          let exprs =
                M.fromList
                  [ ("id", (Nothing, unsafeParseExpr "\\a -> a" $> mempty)),
                    ("const", (Nothing, unsafeParseExpr "\\a -> \\b -> a" $> mempty))
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
            exprs = M.fromList [("a", (Nothing, unsafeParseExpr "1" $> mempty))]
            expectedModule = mempty {moDataTypes = dts, moExpressions = exprs}
         in checkModule' "type Maybe a = Just a | Nothing\ndef a = 1"
              `shouldBe` Right expectedModule

    describe "definitions with types" $ do
      it "function with full signature" $
        let exprs =
              M.fromList
                [ ( "const",
                    ( Just $ unsafeParseMonoType "String -> Int -> String" $> mempty,
                      unsafeParseExpr "\\a -> \\b -> a" $> mempty
                    )
                  )
                ]
            expectedModule = mempty {moExpressions = exprs}
         in checkModule' "def const (a: String) (b: Int) : String = a"
              `shouldBe` Right expectedModule
      it "function with signature where not all args have names" $
        let exprs =
              M.fromList
                [ ( "returnFunc",
                    ( Just (unsafeParseMonoType "String -> Int -> String" $> mempty),
                      unsafeParseExpr "\\a -> const a" $> mempty
                    )
                  )
                ]
            expectedModule = mempty {moExpressions = exprs}
         in checkModule' "def returnFunc (a: String) : Int -> String = const a"
              `shouldBe` Right expectedModule
      it "function where signature has partial types" $
        -- here we just use the identifier as a type arg
        -- feel this could blow up in our faces tbh
        -- but will have to see how it works in practice
        let exprs =
              M.fromList
                [ ( "const",
                    ( Just $ unsafeParseMonoType "String -> b -> String" $> mempty,
                      unsafeParseExpr "\\a -> \\b -> a" $> mempty
                    )
                  )
                ]
            expectedModule = mempty {moExpressions = exprs}
         in checkModule' "def const (a: String) b : String = a"
              `shouldBe` Right expectedModule
      it "function where signature has partial types but no return" $
        -- here we add a placeholder return type
        -- again, not sure about this, see how it works in practice
        -- might need to add a specific type checker notion of `dont know`
        -- that is turned into a unification variable once found
        let exprs =
              M.fromList
                [ ( "const",
                    ( Just (unsafeParseMonoType "String -> b -> returnType" $> mempty),
                      unsafeParseExpr "\\a -> \\b -> a" $> mempty
                    )
                  )
                ]
            expectedModule = mempty {moExpressions = exprs}
         in checkModule' "def const (a: String) b = a"
              `shouldBe` Right expectedModule
      it "multiple functions with signatures" $
        let exprs =
              M.fromList
                [ ( "fmap",
                    ( Just (unsafeParseMonoType "(a -> b) -> Maybe a -> Maybe b" $> mempty),
                      unsafeParseExpr "\\f -> \\maybeA -> match maybeA with Just a -> Just (f a) | Nothing -> Nothing" $> mempty
                    )
                  ),
                  ( "inc",
                    (Nothing, unsafeParseExpr "\\a -> a + 1" $> mempty)
                  )
                ]
            expectedModule =
              mempty
                { moExpressions = exprs
                }
         in checkModule' "def fmap (f: a -> b) (maybeA: Maybe a): Maybe b = match maybeA with Just a -> Just (f a) | Nothing -> Nothing\n\n\ndef inc a = a + 1"
              `shouldBe` Right expectedModule
