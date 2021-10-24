{-# LANGUAGE OverloadedStrings #-}

module Test.Backend.Typescript
  ( spec,
  )
where

import Control.Monad.Except
import Data.Bifunctor
import Data.Foldable
import Data.Functor
import Data.Hashable
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.Mimsa.Actions.Shared as Actions
import Language.Mimsa.Backend.Runtimes
import Language.Mimsa.Backend.Typescript.DataType
import Language.Mimsa.Backend.Typescript.FromExpr
import Language.Mimsa.Backend.Typescript.Monad
import Language.Mimsa.Backend.Typescript.Patterns
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
import Test.Utils.Compilation
import Test.Utils.Helpers
import Test.Utils.Serialisation

testFromExpr :: Expr Name MonoType -> (TSModule, Text)
testFromExpr expr =
  let readerState = TSReaderState mempty
   in case fromExpr readerState expr of
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
      let readerState = TSReaderState mempty
      first prettyPrint (printModule <$> fromExpr readerState exprName)

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

-- test that we have a valid Typescript module by saving it and running it
testTypescriptFileInNode :: FilePath -> IO String
testTypescriptFileInNode tsFilename = do
  -- create output
  (ec, err) <- runTypescriptFromFile tsFilename
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

fullTestIt :: (Text, String) -> Spec
fullTestIt (input, expectedValue) =
  it (T.unpack input) $ do
    let unsafeParse = ($> mempty) . unsafeParseExpr
        expr = unsafeParse input
    filename <- testProjectCompile tsConsoleRuntime expr
    result <- testTypescriptFileInNode filename
    result `shouldBe` expectedValue

-- | input, output TS, nodeJS output
testCases :: [(Text, Text, String)]
testCases =
  [ ("True", "export const main = true", "true"),
    ("False", "export const main = false", "false"),
    ("123", "export const main = 123", "123"),
    ("\"Poo\"", "export const main = \"Poo\"", "Poo"),
    ( "\\a -> a",
      "export const main = <A>(a: A) => a",
      "[Function (anonymous)]"
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
    ( "\\a -> let b = 123 in a",
      "export const main = <A>(a: A) => { const b = 123; return a; }",
      "[Function (anonymous)]"
    ),
    ("(1,2)", "export const main = [1,2]", "[ 1, 2 ]"),
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
    ( "let (a, b) = (1,2) in a",
      "const [a,b] = [1,2]; export const main = a",
      "1"
    ),
    ( "let { dog: a, cat: b } = { dog: 1, cat: 2} in (a,b)",
      "const { cat: b, dog: a } = { cat: 2, dog: 1 }; export const main = [a,b]",
      "[ 1, 2 ]"
    )
  ]

-- | input, nodeJS output
fullTestCases :: [(Text, String)]
fullTestCases =
  [ ("True", "true"),
    ("False", "false"),
    ("123", "123"),
    ("\"Poo\"", "Poo"),
    ("id", "[Function (anonymous)]"),
    ( "\\a -> a",
      "[Function (anonymous)]"
    ),
    ( "id 1",
      "1"
    ),
    ( "if True then 1 else 2",
      "1"
    ),
    ( "let a = \"dog\" in 123",
      "123"
    ),
    ( "let a = \"dog\" in let b = \"horse\" in 123",
      "123"
    ),
    ( "{ a: 123, b: \"horse\" }",
      "{ a: 123, b: 'horse' }"
    ),
    ( "let (a,b) = aPair in a",
      "1"
    ),
    ( "\\a -> let b = 123 in a",
      "[Function (anonymous)]"
    ),
    ("(1,2)", "[ 1, 2 ]"),
    ("aRecord.a", "1"),
    ( "Just",
      "[Function (anonymous)]"
    ),
    ( "Just 1",
      "{ type: 'Just', vars: [ 1 ] }"
    ),
    ( "Nothing",
      "{ type: 'Nothing', vars: [] }"
    ),
    ( "These",
      "[Function (anonymous)]"
    ),
    ("True == True", "true"),
    ("2 + 2", "4"),
    ("10 - 2", "8"),
    ( "\"dog\" ++ \"log\"",
      "doglog"
    ),
    ( "{ fn: (\\a -> let d = 1 in a) }",
      "{ fn: [Function: fn] }"
    ),
    ( "[1,2] <> [3,4]",
      "[ 1, 2, 3, 4 ]"
    ),
    ( "match Just True with (Just a) -> a | _ -> False",
      "true"
    ),
    ( "match Just True with (Just a) -> Just a | _ -> Nothing",
      "{ type: 'Just', vars: [ true ] }"
    ),
    ( "match Just True with (Just a) -> let b = 1; Just a | _ -> Nothing",
      "{ type: 'Just', vars: [ true ] }"
    ),
    ( "let (a, b) = (1,2) in a",
      "1"
    ),
    ( "let { dog: a, cat: b } = { dog: 1, cat: 2} in (a,b)",
      "[ 1, 2 ]"
    ),
    ( "let (Ident a) = Ident 1 in a",
      "1"
    ),
    ( "let (Pair a b) = Pair 1 2 in (a,b)",
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
              (TSType Nothing "boolean" [])
              Nothing
              (TSFunctionBody (TSBody mempty (TSLit (TSInt 1))))
          )
          `shouldBe` "(a: boolean) => 1"
        prettyPrint
          ( TSFunction
              "maybeA"
              (S.singleton (TSGeneric "A"))
              (TSType (Just "Maybe") "Maybe" [TSTypeVar "A"])
              Nothing
              (TSFunctionBody (TSBody mempty (TSLit (TSInt 1))))
          )
          `shouldBe` "<A>(maybeA: Maybe.Maybe<A>) => 1"
        prettyPrint
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
      describe "patterns" $ do
        it "destructure" $ do
          let destructure' = prettyPrint . destructure
          destructure' (TSPatternVar "a") `shouldBe` "const a = value; "
          destructure' TSPatternWildcard `shouldBe` ""
          destructure'
            ( TSPatternPair
                (TSPatternVar "a")
                (TSPatternVar "b")
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
            `shouldBe` "const d = value.charAt(0); \nconst og = value.slice(1); "
          destructure' (TSPatternConstructor "Just" [TSPatternString (TSStringVar "d") (TSStringVar "og")])
            `shouldBe` "const d = value.vars[0].charAt(0); \nconst og = value.vars[0].slice(1); "

        it "conditions" $ do
          let conditions' = prettyPrint . conditions
          conditions' (TSPatternVar "a") `shouldBe` "true"
          conditions' TSPatternWildcard `shouldBe` "true"
          conditions'
            ( TSPatternPair
                (TSPatternLit (TSInt 11))
                (TSPatternLit (TSInt 23))
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
            `shouldBe` "value.type === \"Just\" && value.vars[0] === true"
          conditions' (TSPatternConstructor "Just" [TSPatternWildcard])
            `shouldBe` "value.type === \"Just\""
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
        testFromInputText "\\a -> match a with [a1,...as] -> [as] | [] -> []"
          `shouldBe` Right "export const main = <C>(a: C[]) => { const match = (value: C[]) => { if (value.length >= 1) { const [a1,...as] = value; return [as]; }; if (value.length === 0) { return []; }; throw new Error(\"Pattern match error\"); }; return match(a); }"

    describe "Entire compilation" $ do
      traverse_ fullTestIt fullTestCases

      let unsafeParse = ($> mempty) . unsafeParseExpr
      it "Compiles the smallest project" $ do
        let expr = MyLiteral mempty (MyString (StringType "hello world"))
        filename <- testProjectCompile tsConsoleRuntime expr
        result <- testTypescriptFileInNode filename
        result `shouldBe` "hello world"

      it "Compiles project with one dependency" $ do
        let expr = unsafeParse "id \"hello again\""
        filename <- testProjectCompile tsConsoleRuntime expr
        result <- testTypescriptFileInNode filename
        result `shouldBe` "hello again"

      it "Compiles project with declared type" $ do
        let expr =
              unsafeParse
                "type Id a = Id a; let (Id aaa) = Id \"dog\" in aaa"
        filename <- testProjectCompile tsConsoleRuntime expr
        result <- testTypescriptFileInNode filename
        result `shouldBe` "dog"

      it "Compiles project with type dependency" $ do
        let expr =
              unsafeParse
                "let str = \"hey\" in match (Just str) with (Just a) -> a | _ -> \"\""
        filename <- testProjectCompile tsConsoleRuntime expr
        result <- testTypescriptFileInNode filename
        result `shouldBe` "hey"
