{-# LANGUAGE OverloadedStrings #-}

module Test.Backend.ESModulesJS
  ( spec,
  )
where

-- these are only the unit tests for ES modules output, testing individual
-- expressions

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.Mimsa.Backend.Javascript.Printer as JS
import Language.Mimsa.Backend.Typescript.DataType
import Language.Mimsa.Backend.Typescript.FromExpr
import Language.Mimsa.Backend.Typescript.Monad
import Language.Mimsa.Backend.Typescript.Patterns
import Language.Mimsa.Backend.Typescript.Types
import Language.Mimsa.Core

import Test.Hspec
import Test.Utils.Helpers

testFromExpr :: Expr Name MonoType -> (TSModule, Text)
testFromExpr expr =
  let readerState = TSReaderState mempty mempty
      startState = TSCodegenState mempty mempty mempty
   in case fromExpr readerState startState expr of
        Right (ejsModule, _) -> (ejsModule, JS.printModule ejsModule)
        Left e -> error (T.unpack (prettyPrint e))

spec :: Spec
spec = do
  describe "ESModulesJS" $ do
    describe "pretty print AST" $ do
      it "literals" $ do
        JS.printLiteral (TSBool True) `shouldBe` "true"
        JS.printLiteral (TSInt 100) `shouldBe` "100"
        JS.printLiteral (TSString "egg") `shouldBe` "`egg`"
      it "function" $ do
        JS.printExpr
          ( TSFunction
              "a"
              mempty
              (TSType Nothing "boolean" [])
              Nothing
              (TSFunctionBody (TSBody mempty (TSLit (TSInt 1))))
          )
          `shouldBe` "(a) => 1"
        JS.printExpr
          ( TSFunction
              "maybeA"
              (S.singleton (TSGeneric "A"))
              (TSType (Just "Maybe") "Maybe" [TSTypeVar "A"])
              Nothing
              (TSFunctionBody (TSBody mempty (TSLit (TSInt 1))))
          )
          `shouldBe` "(maybeA) => 1"
        JS.printExpr
          ( TSFunction
              "maybeA"
              (S.singleton (TSGeneric "A"))
              (TSType (Just "Maybe") "Maybe" [TSTypeVar "A"])
              Nothing
              ( TSFunctionBody
                  ( TSBody
                      [ TSAssignment
                          (TSVar "b")
                          Nothing
                          (TSLetBody (TSBody [] (TSLit (TSBool True))))
                      ]
                      (TSLit (TSInt 1))
                  )
              )
          )
          `shouldBe` "(maybeA) => { const b = true; return 1; }"
      it "function application" $ do
        JS.printExpr (TSApp (TSVar "id") (TSLit (TSBool True)))
          `shouldBe` "id(true)"
        JS.printExpr (TSApp (TSApp (TSVar "id") (TSLit (TSBool True))) (TSLit (TSInt 1)))
          `shouldBe` "id(true)(1)"
      it "infix operators" $ do
        JS.printExpr (TSInfix TSEquals (TSLit (TSInt 1)) (TSLit (TSInt 2)))
          `shouldBe` "1 === 2"
      it "record" $ do
        JS.printExpr
          ( TSRecord
              ( M.fromList
                  [ ( "a",
                      TSLit (TSInt 1)
                    ),
                    ("b", TSLit (TSBool True))
                  ]
              )
          )
          `shouldBe` "{ a: 1, b: true }"
      it "record access" $ do
        JS.printExpr (TSRecordAccess "a" (TSVar "record")) `shouldBe` "record.a"
      it "array" $ do
        JS.printExpr
          ( TSArray
              [ TSArrayItem (TSLit (TSInt 1)),
                TSArrayItem (TSLit (TSInt 2)),
                TSArraySpread (TSVar "rest")
              ]
          )
          `shouldBe` "[1,2,...rest]"
      it "array access" $ do
        JS.printExpr (TSArrayAccess 2 (TSVar "array"))
          `shouldBe` "array[2]"
      it "ternary" $ do
        JS.printExpr
          ( TSTernary
              (TSLit (TSBool True))
              (TSLit (TSInt 1))
              (TSLit (TSInt 2))
          )
          `shouldBe` "true ? 1 : 2"
      describe "patterns" $ do
        it "destructure" $ do
          let destructure' = mconcat . fmap JS.printStatement . destructure
          destructure' (TSPatternVar "a") `shouldBe` "const a = value; "
          destructure' TSPatternWildcard `shouldBe` ""
          destructure'
            ( TSPatternTuple
                [ TSPatternVar "a",
                  TSPatternVar "b"
                ]
            )
            `shouldBe` "const [a,b] = value; "
          destructure'
            ( TSPatternRecord
                ( M.fromList
                    [("a", TSPatternVar "a"), ("b", TSPatternVar "b")]
                )
            )
            `shouldBe` "const { a: a, b: b } = value; "
          destructure' (TSPatternConstructor "Just" [TSPatternVar "a"])
            `shouldBe` "const { vars: [a] } = value; "
          destructure' (TSPatternConstructor "Just" [TSPatternWildcard])
            `shouldBe` ""
          destructure' (TSPatternString (TSStringVar "d") (TSStringVar "og"))
            `shouldBe` "const d = value.charAt(0); const og = value.slice(1); "
          destructure' (TSPatternConstructor "Just" [TSPatternString (TSStringVar "d") (TSStringVar "og")])
            `shouldBe` "const d = value.vars[0].charAt(0); const og = value.vars[0].slice(1); "

        it "conditions" $ do
          let conditions' = JS.printExpr . conditions
          conditions' (TSPatternVar "a") `shouldBe` "true"
          conditions' TSPatternWildcard `shouldBe` "true"
          conditions'
            ( TSPatternTuple
                [ TSPatternLit (TSInt 11),
                  TSPatternLit (TSInt 23)
                ]
            )
            `shouldBe` "value[0] === 11 && value[1] === 23"
          conditions'
            ( TSPatternRecord
                ( M.fromList
                    [("a", TSPatternLit (TSInt 11)), ("b", TSPatternVar "b")]
                )
            )
            `shouldBe` "value.a === 11"
          conditions' (TSPatternConstructor "Just" [TSPatternLit (TSBool True)])
            `shouldBe` "value.type === `Just` && value.vars[0] === true"
          conditions' (TSPatternConstructor "Just" [TSPatternWildcard])
            `shouldBe` "value.type === `Just`"
          conditions' (TSPatternString (TSStringVar "d") (TSStringVar "og"))
            `shouldBe` "value.length >= 1"

      it "top level module" $ do
        JS.printModule (TSModule mempty (TSBody mempty (TSLit (TSBool True))))
          `shouldBe` "export const main = true"
        JS.printModule
          ( TSModule
              mempty
              ( TSBody
                  [ TSAssignment
                      (TSVar "a")
                      Nothing
                      (TSLetBody (TSBody mempty (TSLit (TSBool True))))
                  ]
                  (TSVar "a")
              )
          )
          `shouldBe` "const a = true; export const main = a"
    describe "from typed expression" $ do
      it "const bool" $
        testFromExpr (MyLiteral mtBool (MyBool True))
          `shouldBe` ( TSModule mempty (TSBody [] (TSLit (TSBool True))),
                       "export const main = true"
                     )

      it "let a = true in a" $
        snd
          ( testFromExpr
              ( MyLet
                  mtBool
                  (Identifier mtBool "a")
                  ( MyLiteral mtBool (MyBool True)
                  )
                  (MyVar mtBool Nothing "a")
              )
          )
          `shouldBe` "const a = true; export const main = a"

      it "let (a,_) = (true,false) in a" $ do
        snd
          ( testFromExpr
              ( MyLetPattern
                  (MTTuple mempty mtBool (NE.singleton mtBool))
                  (PTuple (MTTuple mempty mtBool (NE.singleton mtBool)) (PVar mtBool "a") (NE.singleton $ PWildcard mtBool))
                  ( MyTuple
                      (MTTuple mempty mtBool (NE.singleton mtBool))
                      (MyLiteral mtBool (MyBool True))
                      (NE.singleton $ MyLiteral mtBool (MyBool False))
                  )
                  (MyVar mtBool Nothing "a")
              )
          )
          `shouldBe` "const [a,_] = [true,false]; export const main = a"

      it "function with known type" $ do
        snd
          ( testFromExpr
              ( MyLambda
                  (MTFunction mempty mtString mtString)
                  (Identifier mtString "str")
                  (MyVar mtString Nothing "str")
              )
          )
          `shouldBe` "export const main = (str) => str"
      it "function with generic type used multiple times" $ do
        snd
          ( testFromExpr
              ( MyLambda
                  (MTFunction mempty (mtVar "a") (mtVar "a"))
                  (Identifier (mtVar "a") "a")
                  ( MyLambda
                      (MTFunction mempty (mtVar "a") (mtVar "a"))
                      (Identifier (mtVar "a") "a2")
                      (MyVar (mtVar "a") Nothing "a")
                  )
              )
          )
          `shouldBe` "export const main = (a) => (a2) => a"

      describe "Create constructor functions" $ do
        let tsMaybe =
              TSDataType
                "Maybe"
                ["A"]
                [ TSConstructor "Just" [TSTypeVar "A"],
                  TSConstructor "Nothing" mempty
                ]
            tsThese =
              TSDataType
                "These"
                ["A", "B"]
                [ TSConstructor "This" [TSTypeVar "A"],
                  TSConstructor "That" [TSTypeVar "B"],
                  TSConstructor "These" [TSTypeVar "A", TSTypeVar "B"]
                ]

        it "Maybe" $ do
          JS.printStatement <$> createConstructorFunctions tsMaybe
            `shouldBe` [ "const Just = (a) => ({ type: \"Just\", vars: [a] }); ",
                         "const Nothing = { type: \"Nothing\", vars: [] }; "
                       ]
        it "These" $ do
          JS.printStatement <$> createConstructorFunctions tsThese
            `shouldBe` [ "const This = (a) => ({ type: \"This\", vars: [a] }); ",
                         "const That = (b) => ({ type: \"That\", vars: [b] }); ",
                         "const These = (a) => (b) => ({ type: \"These\", vars: [a,b] }); "
                       ]

