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
import Language.Mimsa.Backend.Typescript.DataType
import Language.Mimsa.Backend.Typescript.FromExpr
import Language.Mimsa.Backend.Typescript.Printer
import Language.Mimsa.Backend.Typescript.Types
import Language.Mimsa.Interpreter.UseSwaps
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Typechecker
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
    Right tsModule -> (tsModule, printModule tsModule)
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
      first prettyPrint (printModule <$> fromExpr exprName)

-- test that we have a valid Typescript module by saving it and running it
testTypescriptInNode :: Text -> IO String
testTypescriptInNode ts = do
  -- write file
  tsPath <- createOutputFolder "Typescript"
  let tsFilename = tsPath <> show (hash ts) <> ".ts"
  -- cache output
  cachePath <- createOutputFolder "Typescript-result"
  let cacheFilename = cachePath <> show (hash ts) <> ".json"
  -- create output
  let tsOutput = ts <> "\nconsole.log(main)"
  writeFile tsFilename (T.unpack tsOutput)
  (ec, err) <- withCache cacheFilename (runTypescriptFromFile tsFilename)
  if ec then pure err else fail err

testIt :: (Text, Text, String) -> Spec
testIt (expr, expectedTS, expectedValue) =
  it (T.unpack expr) $ do
    case testFromInputText expr of
      Left e -> fail (T.unpack e)
      Right ts -> do
        ts `shouldBe` expectedTS
        val <- testTypescriptInNode ts
        val `shouldBe` expectedValue

maybeOutput :: Text
maybeOutput = "export type Maybe<A> = { type: \"Just\", vars: [A] } | { type: \"Nothing\", vars: [] }; export const Just = <A>(a: A): Maybe<A> => ({ type: \"Just\", vars: [a] }); export const Nothing: Maybe<never> = { type: \"Nothing\", vars: [] }; "

theseOutput :: Text
theseOutput = "export type These<A, B> = { type: \"That\", vars: [B] } | { type: \"These\", vars: [A, B] } | { type: \"This\", vars: [A] }; export const That = <B>(b: B): These<never,B> => ({ type: \"That\", vars: [b] }); export const These = <A>(a: A) => <B>(b: B): These<A,B> => ({ type: \"These\", vars: [a,b] }); export const This = <A>(a: A): These<A,never> => ({ type: \"This\", vars: [a] }); "

identOutput :: Text
identOutput = "export type Ident<A> = { type: \"Ident\", vars: [A] }; export const Ident = <A>(a: A): Ident<A> => ({ type: \"Ident\", vars: [a] }); "

pairOutput :: Text
pairOutput = "export type Pair<A, B> = { type: \"Pair\", vars: [A, B] }; export const Pair = <A>(a: A) => <B>(b: B): Pair<A,B> => ({ type: \"Pair\", vars: [a,b] }); "

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
      maybeOutput <> "export const main = Just",
      "[Function (anonymous)]"
    ),
    ( "Just 1",
      maybeOutput <> "export const main = Just(1)",
      "{ type: 'Just', vars: [ 1 ] }"
    ),
    ( "Nothing",
      maybeOutput <> "export const main = Nothing",
      "{ type: 'Nothing', vars: [] }"
    ),
    ( "These",
      theseOutput <> "export const main = These",
      "[Function (anonymous)]"
    ),
    ("True == True", "export const main = true === true", "true"),
    ("2 + 2", "export const main = 2 + 2", "4"),
    ("10 - 2", "export const main = 10 - 2", "8"),
    ( "\"dog\" ++ \"log\"",
      "export const main = \"dog\" + \"log\"",
      "doglog"
    ),
    ( "{ fn: (\\a -> let d = 1 in a) }",
      "export const main = { fn: <A>(a: A) => { const d = 1; return a; } }",
      "{ fn: [Function: fn] }"
    ),
    ( "[1,2] <> [3,4]",
      "export const main = [...[1,2],...[3,4]]",
      "[ 1, 2, 3, 4 ]"
    ),
    ( "match Just True with (Just a) -> a | _ -> False",
      maybeOutput <> "const match = (value: Maybe<boolean>) => { if (value.type === \"Just\") { const { vars: [a] } = value; return a; }; if (true) { return false; }; throw new Error(\"Pattern match error\"); }; export const main = match(Just(true))",
      "true"
    ),
    ( "match Just True with (Just a) -> Just a | _ -> Nothing",
      maybeOutput <> "const match = (value: Maybe<boolean>) => { if (value.type === \"Just\") { const { vars: [a] } = value; return Just(a); }; if (true) { return Nothing; }; throw new Error(\"Pattern match error\"); }; export const main = match(Just(true))",
      "{ type: 'Just', vars: [ true ] }"
    ),
    ( "match Just True with (Just a) -> let b = 1; Just a | _ -> Nothing",
      maybeOutput <> "const match = (value: Maybe<boolean>) => { if (value.type === \"Just\") { const { vars: [a] } = value; const b = 1; return Just(a); }; if (true) { return Nothing; }; throw new Error(\"Pattern match error\"); }; export const main = match(Just(true))",
      "{ type: 'Just', vars: [ true ] }"
    ),
    ( "let (a, b) = (1,2) in a",
      "const [a,b] = [1,2]; export const main = a",
      "1"
    ),
    ( "let { dog: a, cat: b } = { dog: 1, cat: 2} in (a,b)",
      "const { cat: b, dog: a } = { cat: 2, dog: 1 }; export const main = [a,b]",
      "[ 1, 2 ]"
    ),
    ( "let (Ident a) = Ident 1 in a",
      identOutput <> "const { vars: [a] } = Ident(1); export const main = a",
      "1"
    ),
    ( "let (Pair a b) = Pair 1 2 in (a,b)",
      pairOutput <> "const { vars: [a, b] } = Pair(1)(2); export const main = [a,b]",
      "[ 1, 2 ]"
    )
  ]

spec :: Spec
spec = do
  describe "Typescript" $ do
    describe "pretty print Typescript AST" $ do
      it "literals" $ do
        printLiteral (TSBool True) `shouldBe` "true"
        printLiteral (TSInt 100) `shouldBe` "100"
        printLiteral (TSString "egg") `shouldBe` "\"egg\""
      it "function" $ do
        prettyPrint
          ( TSFunction
              "a"
              mempty
              (TSType "boolean" [])
              Nothing
              (TSFunctionBody (TSBody mempty (TSLit (TSInt 1))))
          )
          `shouldBe` "(a: boolean) => 1"
        prettyPrint
          ( TSFunction
              "maybeA"
              (S.singleton (TSGeneric "A"))
              (TSType "Maybe" [TSTypeVar "A"])
              Nothing
              (TSFunctionBody (TSBody mempty (TSLit (TSInt 1))))
          )
          `shouldBe` "<A>(maybeA: Maybe<A>) => 1"
        prettyPrint
          ( TSFunction
              "maybeA"
              (S.singleton (TSGeneric "A"))
              (TSType "Maybe" [TSTypeVar "A"])
              Nothing
              ( TSFunctionBody
                  ( TSBody
                      [ TSAssignment
                          (TSPatternVar "b")
                          Nothing
                          (TSLetBody (TSBody [] (TSLit (TSBool True))))
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
        prettyPrint
          ( TSArray
              [ TSArrayItem (TSLit (TSInt 1)),
                TSArrayItem (TSLit (TSInt 2)),
                TSArraySpread (TSVar "rest")
              ]
          )
          `shouldBe` "[1,2,...rest]"
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
        destructure (TSPatternVar "a") `shouldBe` (True, "a")
        destructure TSPatternWildcard `shouldBe` (False, "_")
        destructure
          ( TSPatternPair
              (TSPatternVar "a")
              (TSPatternVar "b")
          )
          `shouldBe` (True, "[a,b]")
        destructure
          ( TSPatternRecord
              ( M.fromList
                  [("a", TSPatternVar "a"), ("b", TSPatternVar "b")]
              )
          )
          `shouldBe` (True, "{ a: a, b: b }")
        destructure (TSPatternConstructor "Just" [TSPatternVar "a"])
          `shouldBe` (True, "{ vars: [a] }")
        destructure (TSPatternConstructor "Just" [TSPatternWildcard])
          `shouldBe` (False, "{ vars: [_] }")

      it "top level module" $ do
        printModule (TSModule mempty (TSBody mempty (TSLit (TSBool True))))
          `shouldBe` "export const main = true"
        printModule
          ( TSModule
              mempty
              ( TSBody
                  [ TSAssignment
                      (TSPatternVar "a")
                      Nothing
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
                  "a"
                  ( MyLiteral mtBool (MyBool True)
                  )
                  (MyVar mtBool "a")
              )
          )
          `shouldBe` "const a = true; export const main = a"

      it "let (a,_) = (true,false) in a" $ do
        snd
          ( testFromExpr
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
          )
          `shouldBe` "const [a,_] = [true,false]; export const main = a"

      it "function with known type" $ do
        snd
          ( testFromExpr
              (MyLambda (MTFunction mempty mtString mtString) "str" (MyVar mtString "str"))
          )
          `shouldBe` "export const main = (str: string) => str"
      it "function with generic type used multiple times" $ do
        snd
          ( testFromExpr
              ( MyLambda
                  (MTFunction mempty (mtVar "a") (mtVar "a"))
                  "a"
                  ( MyLambda
                      (MTFunction mempty (mtVar "a") (mtVar "a"))
                      "a2"
                      (MyVar (mtVar "a") "a")
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

        it "Maybe" $ do
          prettyPrint <$> createConstructorFunctions tsMaybe
            `shouldBe` [ "const Just = <A>(a: A): Maybe<A> => ({ type: \"Just\", vars: [a] }); ",
                         "const Nothing: Maybe<never> = { type: \"Nothing\", vars: [] }; "
                       ]
        it "These" $ do
          prettyPrint <$> createConstructorFunctions tsThese
            `shouldBe` [ "const This = <A>(a: A): These<A,never> => ({ type: \"This\", vars: [a] }); ",
                         "const That = <B>(b: B): These<never,B> => ({ type: \"That\", vars: [b] }); ",
                         "const These = <A>(a: A) => <B>(b: B): These<A,B> => ({ type: \"These\", vars: [a,b] }); "
                       ]

    describe "from parsed input" $ do
      traverse_ testIt testCases

      it "simple expression" $ do
        testFromInputText "\\a -> a + 100"
          `shouldBe` Right "export const main = (a: number) => a + 100"
      it "pattern matching array spreads" $ do
        testFromInputText "\\a -> match a with [a1,...as] -> Just as | [] -> Nothing"
          `shouldBe` Right ""
