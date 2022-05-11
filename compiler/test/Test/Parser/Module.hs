{-# LANGUAGE OverloadedStrings #-}

module Test.Parser.Module
  ( spec,
  )
where

import Data.Functor
import qualified Data.Map as M
import Data.Text (Text)
import Language.Mimsa.Parser.Helpers
import Language.Mimsa.Parser.Module
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers ()
import Test.Hspec
import Test.Utils.Helpers

testParser :: Text -> Either Text (Module Annotation)
testParser input = do
  let removeAnn mt = mt $> mempty
      doParser i =
        removeAnn <$> parseAndFormat moduleParser i
  doParser input

spec :: Spec
spec =
  fdescribe "Module parser" $ do
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

    describe "testParser" $ do
      it "Empty file" $
        testParser "" `shouldBe` Right mempty
      describe "definitions" $ do
        it "Single constant" $
          let expectedExpr = unsafeParseExpr "100" $> mempty
              exprs = M.singleton "noSig" expectedExpr
              expectedModule = mempty {moExpressions = exprs}
           in testParser "def noSig = 100"
                `shouldBe` Right expectedModule
        it "Two constants" $
          let exprs =
                M.fromList
                  [ ("one", unsafeParseExpr "1" $> mempty),
                    ("two", unsafeParseExpr "2" $> mempty)
                  ]
              expectedModule = mempty {moExpressions = exprs}
           in testParser "def one = 1\ndef two = 2"
                `shouldBe` Right expectedModule
        it "id Function" $
          let exprs =
                M.fromList
                  [ ("id", unsafeParseExpr "\\a -> a" $> mempty)
                  ]
              expectedModule = mempty {moExpressions = exprs}
           in testParser "def id a = a"
                `shouldBe` Right expectedModule
        it "const Function" $
          let exprs =
                M.fromList
                  [ ("const", unsafeParseExpr "\\a -> \\b -> a" $> mempty)
                  ]
              expectedModule = mempty {moExpressions = exprs}
           in testParser "def const a b = a"
                `shouldBe` Right expectedModule
        it "multiple Functions" $
          let exprs =
                M.fromList
                  [ ("id", unsafeParseExpr "\\a -> a" $> mempty),
                    ("const", unsafeParseExpr "\\a -> \\b -> a" $> mempty)
                  ]
              expectedModule = mempty {moExpressions = exprs}
           in testParser "def id a = a\ndef const a b = a"
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
         in testParser "type Maybe a = Just a | Nothing"
              `shouldBe` Right expectedModule
    describe "definitions with types" $ do
      it "function with full signature" $
        let exprs =
              M.fromList [("const", unsafeParseExpr "\\a -> \\b -> a" $> mempty)]
            typeSigs =
              M.fromList [("const", unsafeParseMonoType "String -> Int -> String" $> mempty)]
            expectedModule = mempty {moExpressions = exprs, moTypeSignatures = typeSigs}
         in testParser "def const (a: String) (b: Int) : String = a"
              `shouldBe` Right expectedModule
      it "function with signature where not all args have names" $
        let exprs =
              M.fromList [("returnFunc", unsafeParseExpr "\\a -> const a" $> mempty)]
            typeSigs =
              M.fromList [("returnFunc", unsafeParseMonoType "String -> Int -> String" $> mempty)]
            expectedModule = mempty {moExpressions = exprs, moTypeSignatures = typeSigs}
         in testParser "def returnFunc (a: String) : Int -> String = const a"
              `shouldBe` Right expectedModule
      it "function where signature has partial types" $
        -- here we just use the identifier as a type arg
        -- feel this could blow up in our faces tbh
        -- but will have to see how it works in practice
        let exprs =
              M.fromList [("const", unsafeParseExpr "\\a -> \\b -> a" $> mempty)]
            typeSigs =
              M.fromList [("const", unsafeParseMonoType "String -> b -> String" $> mempty)]
            expectedModule = mempty {moExpressions = exprs, moTypeSignatures = typeSigs}
         in testParser "def const (a: String) b : String = a"
              `shouldBe` Right expectedModule
      it "function where signature has partial types but no return" $
        -- here we add a placeholder return type
        -- again, not sure about this, see how it works in practice
        -- might need to add a specific type checker notion of `dont know`
        -- that is turned into a unification variable once found
        let exprs =
              M.fromList [("const", unsafeParseExpr "\\a -> \\b -> a" $> mempty)]
            typeSigs =
              M.fromList [("const", unsafeParseMonoType "String -> b -> returnType" $> mempty)]
            expectedModule = mempty {moExpressions = exprs, moTypeSignatures = typeSigs}
         in testParser "def const (a: String) b = a"
              `shouldBe` Right expectedModule
      it "multiple functions with signatures" $
        let exprs =
              M.fromList
                [ ( "fmap",
                    unsafeParseExpr "\\f -> \\maybeA -> match maybeA with Just a -> Just (f a) | Nothing -> Nothing" $> mempty
                  ),
                  ( "inc",
                    unsafeParseExpr "\\a -> a + 1" $> mempty
                  )
                ]
            typeSigs =
              M.fromList
                [ ( "fmap",
                    unsafeParseMonoType "(a -> b) -> Maybe a -> Maybe b" $> mempty
                  )
                ]
            expectedModule =
              mempty
                { moExpressions = exprs,
                  moTypeSignatures = typeSigs
                }
         in testParser "def fmap (f: a -> b) (maybeA: Maybe a): Maybe b = match maybeA with Just a -> Just (f a) | Nothing -> Nothing\n\n\ndef inc a = a + 1"
              `shouldBe` Right expectedModule
