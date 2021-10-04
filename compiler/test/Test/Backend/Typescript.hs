{-# LANGUAGE OverloadedStrings #-}

module Test.Backend.Typescript
  ( spec,
  )
where

import Control.Monad.Except
import Data.Bifunctor
import Data.Foldable
import Data.Hashable
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.Mimsa.Actions.Shared as Actions
import Language.Mimsa.Backend.Typescript.FromExpr
import Language.Mimsa.Backend.Typescript.Types
import Language.Mimsa.Interpreter.UseSwaps
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Typechecker
import System.Exit
import Test.Backend.RunNode hiding (spec)
import Test.Data.Project
import Test.Hspec
import Test.Utils.Helpers
import Test.Utils.Serialisation
  ( createOutputFolder,
  )

testFromExpr :: Expr Name MonoType -> (TSModule, Text)
testFromExpr expr =
  case fromExpr expr of
    Right tsModule -> (tsModule, prettyPrint tsModule)
    Left e -> error (T.unpack (prettyPrint e))

testFromInputText :: Text -> Either Text Text
testFromInputText input =
  case Actions.evaluateText testStdlib input of
    Left e -> throwError (prettyPrint e)
    Right resolved -> do
      exprName <-
        first
          (prettyPrint . InterpreterErr)
          (useSwaps (reSwaps resolved) (reTypedExpression resolved))
      Right . prettyPrint . fromExpr $ exprName

-- test that we have a valid Typescript module by saving it and running it
testTypescriptInNode :: Text -> IO String
testTypescriptInNode ts = do
  -- write file
  tsPath <- createOutputFolder "Typescript"
  let tsFilename = tsPath <> show (hash ts) <> ".ts"
  let tsOutput = ts <> "\nconsole.log(main)"
  writeFile tsFilename (T.unpack tsOutput)
  (ec, err) <- runTypescriptFromFile tsFilename
  case ec of
    ExitFailure _ -> fail err
    _ -> pure err

testIt :: (Text, Text, String) -> Spec
testIt (expr, expectedTS, expectedValue) =
  it (T.unpack expr) $ do
    case testFromInputText expr of
      Left e -> fail (T.unpack e)
      Right ts -> do
        ts `shouldBe` expectedTS
        val <- testTypescriptInNode ts
        val `shouldBe` expectedValue

-- | input, output TS, nodeJS output
testCases :: [(Text, Text, String)]
testCases =
  [ ("True", "export const main = true", "true"),
    ("False", "export const main = false", "false"),
    ("123", "export const main = 123", "123"),
    ("\"Poo\"", "export const main = \"Poo\"", "Poo"),
    ("id", "const id = <A>(a: A) => a; export const main = id", "[Function: id]"),
    ( "\\a -> a",
      "export const main = <A>(a: A) => a",
      "[Function (anonymous)]"
    ),
    ( "id 1",
      "const id = <A>(a: A) => a; export const main = id(1)",
      "1"
    ),
    ( "if True then 1 else 2",
      "export const main = true ? 1 : 2",
      "1"
    ),
    ( "let a = \"dog\" in 123",
      "const a = \"dog\"; export const main = 123",
      "123"
    ),
    ( "let a = \"dog\" in let b = \"horse\" in 123",
      "const a = \"dog\"; \nconst b = \"horse\"; export const main = 123",
      "123"
    ),
    ( "{ a: 123, b: \"horse\" }",
      "export const main = { a: 123, b: \"horse\" }",
      "{ a: 123, b: 'horse' }"
    ),
    ( "let (a,b) = aPair in a",
      "const aPair = [1,2]; \nconst [a,b] = aPair; export const main = a",
      "1"
    ),
    ( "\\a -> let b = 123 in a",
      "export const main = <A>(a: A) => { const b = 123; return a; }",
      "[Function (anonymous)]"
    ),
    ("(1,2)", "export const main = [1,2]", "[ 1, 2 ]"),
    ("aRecord.a", "const aRecord = { a: 1, b: \"dog\" }; export const main = aRecord.a", "1"),
    ( "Just",
      "type Maybe<A> = { type: \"Just\", vars: [A] } | { type: \"Nothing\", vars: [] }; export const main = <A>(a: A) => ({ type: \"Just\", vars: [a] })",
      "[Function: main]"
    ),
    ( "Just 1",
      "type Maybe<A> = { type: \"Just\", vars: [A] } | { type: \"Nothing\", vars: [] }; export const main = { type: \"Just\", vars: [1] }",
      "{ type: 'Just', vars: [ 1 ] }"
    ),
    ( "Nothing",
      "const main = { type: \"Nothing\", vars: [] };\n",
      "{ type: 'Nothing', vars: [] }"
    ),
    ( "These",
      "const main = a => b => ({ type: \"These\", vars: [a,b] });\n",
      "[Function: main]"
    ),
    ("True == False", "const main = __eq(true, false);\n", "false"),
    ("2 + 2", "const main = 2 + 2;\n", "4"),
    ("10 - 2", "const main = 10 - 2;\n", "8"),
    ( "\"dog\" ++ \"log\"",
      "const main = \"dog\" + \"log\";\n",
      "doglog"
    ),
    ( "{ fn: (\\a -> let d = 1 in a) }",
      "const main = { fn: a => { const d = 1;\nreturn a } };\n",
      "{ fn: [Function: fn] }"
    ),
    ("[1,2] <> [3,4]", "const main = __concat([1, 2], [3, 4]);\n", "[ 1, 2, 3, 4 ]"),
    ( "match Just True with (Just a) -> a | _ -> False",
      "const main = __patternMatch({ type: \"Just\", vars: [true] }, [ [ pat => __eq(pat.type, \"Just\") ? { a: pat.vars[0] } : null, ({ a }) => a ], [ pat => ({}), () => false ] ]);\n",
      "true"
    ),
    ( "match Just True with (Just a) -> Just a | _ -> Nothing",
      "const main = __patternMatch({ type: \"Just\", vars: [true] }, [ [ pat => __eq(pat.type, \"Just\") ? { a: pat.vars[0] } : null, ({ a }) => ({ type: \"Just\", vars: [a] }) ], [ pat => ({}), () => ({ type: \"Nothing\", vars: [] }) ] ]);\n",
      "{ type: 'Just', vars: [ true ] }"
    ),
    ( "match Just True with (Just a) -> let b = 1; Just a | _ -> Nothing",
      "const main = __patternMatch({ type: \"Just\", vars: [true] }, [ [ pat => __eq(pat.type, \"Just\") ? { a: pat.vars[0] } : null, ({ a }) => { const b = 1;\nreturn { type: \"Just\", vars: [a] } } ], [ pat => ({}), () => ({ type: \"Nothing\", vars: [] }) ] ]);\n",
      "{ type: 'Just', vars: [ true ] }"
    ),
    ( "let (a, b) = (1,2) in a",
      "const main = function() { const [a, b] = [1,2];\nreturn a }();\n",
      "1"
    ),
    ( "let { dog: a, cat: b } = { dog: 1, cat: 2} in (a,b)",
      "const main = function() { const { cat: b, dog: a } = { cat: 2, dog: 1 };\nreturn [a,b] }();\n",
      "[ 1, 2 ]"
    ),
    ( "let (Ident a) = Ident 1 in a",
      "const main = function() { const { vars: [a] } = { type: \"Ident\", vars: [1] };\nreturn a }();\n",
      "1"
    ),
    ( "let (Pair a b) = Pair 1 2 in (a,b)",
      "const main = function() { const { vars: [a, b] } = { type: \"Pair\", vars: [1,2] };\nreturn [a,b] }();\n",
      "[ 1, 2 ]"
    )
  ]

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
          `shouldBe` "<A>(maybeA: Maybe<A>) => { const b = true; return 1; }"
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
        prettyPrint (TSModule mempty (TSBody mempty (TSLit (TSBool True))))
          `shouldBe` "export const main = true"
        prettyPrint
          ( TSModule
              mempty
              ( TSBody
                  [ TSAssignment
                      (TSPatternVar "a")
                      (TSLetBody (TSBody mempty (TSLit (TSBool True))))
                  ]
                  (TSVar "a")
              )
          )
          `shouldBe` "const a = true; export const main = a"
    describe "from typed expression" $ do
      let mtBool = MTPrim mempty MTBool
          mtString = MTPrim mempty MTString
          mtVar a = MTVar mempty (tvNamed a)
          mtMaybe = MTConstructor mempty "Maybe"
          mtMaybeString = MTTypeApp mempty mtMaybe mtString

      it "const bool" $
        testFromExpr (MyLiteral mtBool (MyBool True))
          `shouldBe` ( TSModule mempty (TSBody [] (TSLit (TSBool True))),
                       "export const main = true"
                     )
      it "Maybe String" $
        testFromExpr (MyApp mtMaybeString (MyConstructor mtMaybe "Just") (MyLiteral mtString (MyString "dog")))
          `shouldBe` ( TSModule
                         mempty
                         ( TSBody
                             []
                             ( TSData
                                 "Just"
                                 [ TSLit (TSString "dog")
                                 ]
                             )
                         ),
                       "export const main = { type: \"Just\", vars: [\"dog\"] }"
                     )
      it "let a = true in a" $
        testFromExpr
          ( MyLet
              mtBool
              "a"
              ( MyLiteral mtBool (MyBool True)
              )
              (MyVar mtBool "a")
          )
          `shouldBe` ( TSModule
                         mempty
                         ( TSBody
                             [ TSAssignment
                                 (TSPatternVar "a")
                                 (TSLetBody (TSBody mempty (TSLit (TSBool True))))
                             ]
                             (TSVar "a")
                         ),
                       "const a = true; export const main = a"
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
                         mempty
                         ( TSBody
                             [ TSAssignment
                                 (TSPatternPair (TSPatternVar "a") TSPatternWildcard)
                                 (TSLetBody (TSBody mempty (TSArray [TSLit (TSBool True), TSLit (TSBool False)])))
                             ]
                             (TSVar "a")
                         ),
                       "const [a,_] = [true,false]; export const main = a"
                     )
      it "function with known type" $
        do
          testFromExpr
            (MyLambda (MTFunction mempty mtString mtString) "str" (MyVar mtString "str"))
            `shouldBe` ( TSModule
                           mempty
                           ( TSBody
                               []
                               ( TSFunction
                                   "str"
                                   mempty
                                   (TSType "string" [])
                                   ( TSFunctionBody
                                       ( TSBody [] (TSVar "str")
                                       )
                                   )
                               )
                           ),
                         "export const main = (str: string) => str"
                       )
      it "function with generic type used multiple times" $ do
        testFromExpr
          ( MyLambda
              (MTFunction mempty (mtVar "a") (mtVar "a"))
              "a"
              ( MyLambda
                  (MTFunction mempty (mtVar "a") (mtVar "a"))
                  "a2"
                  (MyVar (mtVar "a") "a")
              )
          )
          `shouldBe` ( TSModule
                         mempty
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
        testFromExpr
          ( MyPatternMatch
              mtBool
              ( MyApp
                  mtMaybeString
                  ( MyConstructor mtMaybe "Just"
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
                         mempty
                         ( TSBody
                             [ TSAssignment
                                 (TSPatternVar "match")
                                 ( TSLetBody
                                     ( TSBody
                                         []
                                         ( TSFunction
                                             "value"
                                             mempty
                                             (TSType "Maybe" [TSType "string" []])
                                             ( TSFunctionBody
                                                 ( TSBody
                                                     [ TSConditional
                                                         (TSPatternConstructor "Just" [TSPatternVar "aa"])
                                                         ( TSLetBody
                                                             ( TSBody
                                                                 [ TSAssignment
                                                                     (TSPatternConstructor "Just" [TSPatternVar "aa"])
                                                                     ( TSLetBody
                                                                         ( TSBody [] (TSVar "value")
                                                                         )
                                                                     )
                                                                 ]
                                                                 (TSVar "aa")
                                                             )
                                                         ),
                                                       TSConditional
                                                         TSPatternWildcard
                                                         ( TSLetBody
                                                             ( TSBody
                                                                 [ TSAssignment
                                                                     TSPatternWildcard
                                                                     (TSLetBody (TSBody [] (TSVar "value")))
                                                                 ]
                                                                 (TSLit (TSString "nope"))
                                                             )
                                                         )
                                                     ]
                                                     (TSError "Pattern match error")
                                                 )
                                             )
                                         )
                                     )
                                 )
                             ]
                             ( TSApp
                                 (TSVar "match")
                                 ( TSData "Just" [TSLit (TSString "dog")]
                                 )
                             )
                         ),
                       "const match = (value: Maybe<string>) => { if (value.type === \"Just\") { const { vars: [aa] } = value; return aa; }; if (true) { const _ = value; return \"nope\"; }; throw new Error(\"Pattern match error\"); }; export const main = match({ type: \"Just\", vars: [\"dog\"] })"
                     )

    describe "from parsed input" $ do
      traverse_ testIt testCases

      it "simple expression" $ do
        testFromInputText "\\a -> a + 100" `shouldBe` Right ""
      xit "pattern matching array spreads" $ do
        testFromInputText "\\a -> match a with [a1,...as] -> Just as | [] -> Nothing"
          `shouldBe` Right ""
