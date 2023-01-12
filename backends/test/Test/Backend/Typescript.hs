{-# LANGUAGE OverloadedStrings #-}

module Test.Backend.Typescript
  ( spec,
  )
where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Backend.Typescript.DataType
import Language.Mimsa.Backend.Typescript.FromExpr
import Language.Mimsa.Backend.Typescript.Monad
import Language.Mimsa.Backend.Typescript.Patterns
import Language.Mimsa.Backend.Typescript.Printer
import Language.Mimsa.Backend.Typescript.Types
import Language.Mimsa.Core
import Test.Hspec
import Test.Utils.Helpers

testFromExpr :: Expr Name MonoType -> (TSModule, Text)
testFromExpr expr =
  let readerState =
        TSReaderState (M.fromList [("Dog", "Pet"), ("Same", "Same")]) mempty
      startState = TSCodegenState mempty mempty mempty
   in case fromExpr readerState startState expr of
        Right (tsModule, _) -> (tsModule, printModule tsModule)
        Left e -> error (T.unpack (prettyPrint e))

spec :: Spec
spec = do
  describe "Typescript" $ do
    describe "pretty print Typescript AST" $ do
      it "literals" $ do
        printLiteral (TSBool True) `shouldBe` "true"
        printLiteral (TSInt 100) `shouldBe` "100"
        printLiteral (TSString "egg") `shouldBe` "`egg`"
      it "function" $ do
        printExpr
          ( TSFunction
              "a"
              mempty
              (TSType Nothing "boolean" [])
              Nothing
              (TSFunctionBody (TSBody mempty (TSLit (TSInt 1))))
          )
          `shouldBe` "(a: boolean) => 1"
        printExpr
          ( TSFunction
              "maybeA"
              (S.singleton (TSGeneric "A"))
              (TSType (Just "Maybe") "Maybe" [TSTypeVar "A"])
              Nothing
              (TSFunctionBody (TSBody mempty (TSLit (TSInt 1))))
          )
          `shouldBe` "<A>(maybeA: Maybe.Maybe<A>) => 1"
        printExpr
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
          `shouldBe` "<A>(maybeA: Maybe.Maybe<A>) => { const b = true; return 1; }"
      it "function application" $ do
        printExpr (TSApp (TSVar "id") (TSLit (TSBool True)))
          `shouldBe` "id(true)"
        printExpr (TSApp (TSApp (TSVar "id") (TSLit (TSBool True))) (TSLit (TSInt 1)))
          `shouldBe` "id(true)(1)"
      it "infix operators" $ do
        printExpr (TSInfix TSEquals (TSLit (TSInt 1)) (TSLit (TSInt 2)))
          `shouldBe` "1 === 2"
      it "record" $ do
        printExpr
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
      it "pair" $ do
        printExpr (TSTuple [TSLit (TSInt 1), TSLit (TSInt 2)]) `shouldBe` "[1,2] as const"
      it "record access" $ do
        printExpr (TSRecordAccess "a" (TSVar "record")) `shouldBe` "record.a"
      it "array" $ do
        printExpr
          ( TSArray
              [ TSArrayItem (TSLit (TSInt 1)),
                TSArrayItem (TSLit (TSInt 2)),
                TSArraySpread (TSVar "rest")
              ]
          )
          `shouldBe` "[1,2,...rest]"
      it "array access" $ do
        printExpr (TSArrayAccess 2 (TSVar "array"))
          `shouldBe` "array[2]"
      it "ternary" $ do
        printExpr
          ( TSTernary
              (TSLit (TSBool True))
              (TSLit (TSInt 1))
              (TSLit (TSInt 2))
          )
          `shouldBe` "true ? 1 : 2"
      describe "patterns" $ do
        it "destructure" $ do
          let destructure' = mconcat . fmap printStatement . destructure
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
          let conditions' = printExpr . conditions
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
        printModule (TSModule mempty (TSBody mempty (TSLit (TSBool True))))
          `shouldBe` "export const main = true"
        printModule
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
      it "Namespaced constructor" $ do
        testFromExpr (MyConstructor mtBool Nothing "Dog")
          `shouldBe` ( TSModule mempty (TSBody [] $ TSRecordAccess "Dog" (TSVar "Pet")),
                       "export const main = Pet.Dog"
                     )

      it "Namespaced constructor with same" $ do
        testFromExpr (MyConstructor mtBool Nothing "Same")
          `shouldBe` ( TSModule mempty (TSBody [] $ TSRecordAccess "Same" (TSVar "Same")),
                       "export const main = Same.Same"
                     )

      it "Namespaced constructor with blah" $ do
        testFromExpr (MyConstructor mtBool (Just "distraction") "Dog")
          `shouldBe` ( TSModule mempty (TSBody [] $ TSRecordAccess "Dog" (TSVar "Pet")),
                       "export const main = Pet.Dog"
                     )

      it "Not namespaced constructor" $ do
        testFromExpr (MyConstructor mtBool Nothing "Log")
          `shouldBe` (TSModule mempty (TSBody [] (TSVar "Log")), "export const main = Log")

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
          `shouldBe` "const [a,_] = [true,false] as const; export const main = a"

      it "function with known type" $ do
        snd
          ( testFromExpr
              ( MyLambda
                  (MTFunction mempty mtString mtString)
                  (Identifier mtString "str")
                  (MyVar mtString Nothing "str")
              )
          )
          `shouldBe` "export const main = (str: string) => str"
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
          `shouldBe` "export const main = <A>(a: A) => (a2: A) => a"

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
            tsMonoid =
              TSDataType
                "Monoid"
                ["A"]
                [ TSConstructor
                    "Monoid"
                    [ TSTypeFun
                        "arg"
                        (TSTypeVar "A")
                        (TSTypeFun "arg" (TSTypeVar "A") (TSTypeVar "A")),
                      TSTypeVar "A"
                    ]
                ]

        it "Maybe" $ do
          printStatement <$> createConstructorFunctions tsMaybe
            `shouldBe` [ "const Just = <A>(a: A): Maybe<A> => ({ type: \"Just\", vars: [a] }); ",
                         "const Nothing: Maybe<never> = { type: \"Nothing\", vars: [] }; "
                       ]
        it "These" $ do
          printStatement <$> createConstructorFunctions tsThese
            `shouldBe` [ "const This = <A>(a: A): These<A,never> => ({ type: \"This\", vars: [a] }); ",
                         "const That = <B>(b: B): These<never,B> => ({ type: \"That\", vars: [b] }); ",
                         "const These = <A>(a: A) => <B>(b: B): These<A,B> => ({ type: \"These\", vars: [a,b] }); "
                       ]
        it "Monoid" $ do
          printStatement <$> createConstructorFunctions tsMonoid
            `shouldBe` [ "const Monoid = <A>(u1: (arg: A) => (arg: A) => A) => (a: A): Monoid<A> => ({ type: \"Monoid\", vars: [u1,a] }); "
                       ]
