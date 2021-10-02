{-# LANGUAGE OverloadedStrings #-}

module Test.Backend.Typescript
  ( spec,
  )
where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Backend.Typescript.FromExpr
import Language.Mimsa.Backend.Typescript.Types
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker
import Test.Hspec
import Test.Utils.Helpers

testFromExpr :: Expr Name MonoType -> (TSModule, Text)
testFromExpr expr =
  let tsModule = fromExpr expr
   in (tsModule, prettyPrint tsModule)

unlines' :: [Text] -> Text
unlines' = T.stripEnd . T.unlines

spec :: Spec
spec = do
  fdescribe "Typescript" $ do
    describe "pretty print Typescript AST" $ do
      it "literals" $ do
        prettyPrint (TSBool True) `shouldBe` "true"
        prettyPrint (TSInt 100) `shouldBe` "100"
        prettyPrint (TSString "egg") `shouldBe` "\"egg\""
      it "function" $ do
        prettyPrint
          ( TSFunction
              "a"
              mempty
              (TSType "boolean" [])
              (TSFunctionBody (TSBody mempty (TSLit (TSInt 1))))
          )
          `shouldBe` "(a: boolean) => 1"
        prettyPrint
          ( TSFunction
              "maybeA"
              (S.singleton (TSGeneric "A"))
              (TSType "Maybe" [TSTypeVar "A"])
              (TSFunctionBody (TSBody mempty (TSLit (TSInt 1))))
          )
          `shouldBe` "<A>(maybeA: Maybe<A>) => 1"
        prettyPrint
          ( TSFunction
              "maybeA"
              (S.singleton (TSGeneric "A"))
              (TSType "Maybe" [TSTypeVar "A"])
              ( TSFunctionBody
                  ( TSBody
                      [ TSAssignment (TSPatternVar "b") (TSLetBody (TSBody [] (TSLit (TSBool True))))
                      ]
                      (TSLit (TSInt 1))
                  )
              )
          )
          `shouldBe` "<A>(maybeA: Maybe<A>) => { const b = true; return 1 }"
      it "function application" $ do
        prettyPrint (TSApp (TSVar "id") (TSLit (TSBool True)))
          `shouldBe` "id(true)"
        prettyPrint (TSApp (TSApp (TSVar "id") (TSLit (TSBool True))) (TSLit (TSInt 1)))
          `shouldBe` "id(true)(1)"
      it "infix operators" $ do
        prettyPrint (TSInfix TSEquals (TSLit (TSInt 1)) (TSLit (TSInt 2)))
          `shouldBe` "1 === 2"
      it "record" $ do
        prettyPrint
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
        prettyPrint (TSRecordAccess "a" (TSVar "record")) `shouldBe` "record.a"
      it "array" $ do
        prettyPrint (TSArray [TSLit (TSInt 1), TSLit (TSInt 2), TSLit (TSInt 3)])
          `shouldBe` "[1,2,3]"
      it "array access" $ do
        prettyPrint (TSArrayAccess 2 (TSVar "array"))
          `shouldBe` "array[2]"
      it "ternary" $ do
        prettyPrint
          ( TSTernary
              (TSLit (TSBool True))
              (TSLit (TSInt 1))
              (TSLit (TSInt 2))
          )
          `shouldBe` "true ? 1 : 2"
      it "patterns" $ do
        destructure (TSPatternVar "a") `shouldBe` "a"
        destructure TSPatternWildcard `shouldBe` "_"
        destructure
          ( TSPatternPair
              (TSPatternVar "a")
              (TSPatternVar "b")
          )
          `shouldBe` "[a,b]"
        destructure
          ( TSPatternRecord
              ( M.fromList
                  [("a", TSPatternVar "a"), ("b", TSPatternVar "b")]
              )
          )
          `shouldBe` "{ a: a, b: b }"
        destructure (TSPatternConstructor "Just" [TSPatternVar "a"])
          `shouldBe` "{ vars: [a] }"
      it "top level module" $ do
        prettyPrint (TSModule (TSBody mempty (TSLit (TSBool True))))
          `shouldBe` "export const main = true"
        prettyPrint
          ( TSModule
              ( TSBody
                  [ TSAssignment
                      (TSPatternVar "a")
                      (TSLetBody (TSBody mempty (TSLit (TSBool True))))
                  ]
                  (TSVar "a")
              )
          )
          `shouldBe` "const a = true\nexport const main = a"
    describe "from typed expression" $ do
      let mtBool = MTPrim mempty MTBool
          mtString = MTPrim mempty MTString
          mtVar a = MTVar mempty (tvNamed a)
      it "const bool" $ do
        testFromExpr (MyLiteral mtBool (MyBool True))
          `shouldBe` ( TSModule (TSBody [] (TSLit (TSBool True))),
                       "export const main = true"
                     )
      it "let a = true in a" $ do
        testFromExpr
          ( MyLet
              mtBool
              "a"
              ( MyLiteral mtBool (MyBool True)
              )
              (MyVar mtBool "a")
          )
          `shouldBe` ( TSModule
                         ( TSBody
                             [ TSAssignment
                                 (TSPatternVar "a")
                                 (TSLetBody (TSBody mempty (TSLit (TSBool True))))
                             ]
                             (TSVar "a")
                         ),
                       unlines' ["const a = true", "export const main = a"]
                     )
      it "let (a,_) = (true,false) in a" $ do
        testFromExpr
          ( MyLetPattern
              (MTPair mempty mtBool mtBool)
              (PPair (MTPair mempty mtBool mtBool) (PVar mtBool "a") (PWildcard mtBool))
              ( MyPair
                  (MTPair mempty mtBool mtBool)
                  (MyLiteral mtBool (MyBool True))
                  (MyLiteral mtBool (MyBool False))
              )
              (MyVar mtBool "a")
          )
          `shouldBe` ( TSModule
                         ( TSBody
                             [ TSAssignment
                                 (TSPatternPair (TSPatternVar "a") TSPatternWildcard)
                                 (TSLetBody (TSBody mempty (TSArray [TSLit (TSBool True), TSLit (TSBool False)])))
                             ]
                             (TSVar "a")
                         ),
                       unlines'
                         [ "const [a,_] = [true,false]",
                           "export const main = a"
                         ]
                     )
      it "function with known type" $
        do
          testFromExpr
            (MyLambda mtString "str" (MyVar mtString "str"))
            `shouldBe` ( TSModule
                           ( TSBody
                               []
                               ( TSFunction
                                   "str"
                                   mempty
                                   (TSType "String" [])
                                   ( TSFunctionBody
                                       ( TSBody [] (TSVar "str")
                                       )
                                   )
                               )
                           ),
                         "export const main = (str: String) => str"
                       )
      it "function with generic type used multiple times" $ do
        testFromExpr
          (MyLambda (mtVar "a") "a" (MyLambda (mtVar "a") "a2" (MyVar (mtVar "a") "a")))
          `shouldBe` ( TSModule
                         ( TSBody
                             []
                             ( TSFunction
                                 "a"
                                 (S.singleton (TSGeneric "A"))
                                 (TSTypeVar "A")
                                 ( TSFunctionBody
                                     ( TSBody
                                         []
                                         ( TSFunction
                                             "a2"
                                             mempty
                                             (TSTypeVar "A")
                                             ( TSFunctionBody
                                                 ( TSBody [] (TSVar "a")
                                                 )
                                             )
                                         )
                                     )
                                 )
                             )
                         ),
                       "export const main = <A>(a: A) => (a2: A) => a"
                     )
      it "pattern match" $ do
        let mtMaybe = MTConstructor mempty "Maybe"
            mtMaybeString = MTTypeApp mempty mtMaybe mtString
        testFromExpr
          ( MyPatternMatch
              mtBool
              ( MyApp
                  mtMaybeString
                  ( MyConstructor mtMaybe "Maybe"
                  )
                  (MyLiteral mtString (MyString "dog"))
              )
              [ ( PConstructor
                    mtMaybeString
                    "Just"
                    [ PVar mtString "aa"
                    ],
                  MyVar mtString "aa"
                ),
                ( PWildcard mtMaybeString,
                  MyLiteral mtString (MyString "nope")
                )
              ]
          )
          `shouldBe` ( TSModule
                         ( TSBody
                             [ TSAssignment
                                 (TSPatternVar "match")
                                 ( TSLetBody
                                     ( TSBody
                                         [ TSConditional
                                             (TSPatternConstructor "Just" [TSPatternVar "aa"])
                                             (TSLetBody (TSBody [] (TSVar "aa"))),
                                           TSConditional
                                             TSPatternWildcard
                                             ( TSLetBody
                                                 ( TSBody
                                                     []
                                                     (TSLit (TSString "nope"))
                                                 )
                                             )
                                         ]
                                         (TSError "Pattern match error")
                                     )
                                 )
                             ]
                             ( TSApp
                                 (TSVar "match")
                                 ( TSData "Just" [TSLit (TSString "dog")]
                                 )
                             )
                         ),
                       "export bum"
                     )
