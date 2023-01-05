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
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Backend.Types
import Language.Mimsa.Backend.Typescript.DataType
import Language.Mimsa.Backend.Typescript.FromExpr
import Language.Mimsa.Backend.Typescript.Monad
import Language.Mimsa.Backend.Typescript.Patterns
import Language.Mimsa.Backend.Typescript.Printer
import Language.Mimsa.Backend.Typescript.Types
import Language.Mimsa.Core
import Language.Mimsa.Project.Stdlib
import Test.Backend.RunNode hiding (spec)
import Test.Hspec
import Test.Utils.Compilation
import Test.Utils.Helpers
import Test.Utils.Serialisation

testFromExpr :: Expr Name MonoType -> (TSModule, Text)
testFromExpr expr =
  let readerState =
        TSReaderState (M.fromList [("Dog", "Pet"), ("Same", "Same")]) mempty
      startState = TSCodegenState mempty mempty mempty
   in case fromExpr readerState startState expr of
        Right (tsModule, _) -> (tsModule, printModule tsModule)
        Left e -> error (T.unpack (prettyPrint e))

testFromInputText :: Text -> Either Text Text
testFromInputText input =
  case evaluateText stdlib input of
    Left e -> throwError (prettyPrint e)
    Right typedExpr -> do
      let readerState = TSReaderState mempty mempty
          startState = TSCodegenState mempty mempty mempty
      first
        prettyPrint
        (printModule . fst <$> fromExpr readerState startState typedExpr)

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
    (filename, contentHash) <- testProjectCompile "CompileTSProject" Typescript expr
    cachePath <- createOutputFolder "CompileTSProject-result"
    let cacheFilename = cachePath <> show contentHash <> ".json"

    result <- withCache cacheFilename (testTypescriptFileInNode filename)
    result `shouldBe` expectedValue

testModule :: (Text, String) -> Spec
testModule (input, expectedValue) =
  it (T.unpack input) $ do
    (filename, contentHash) <- testModuleCompile "CompileTSModuleProject" Typescript input
    cachePath <- createOutputFolder "CompileTSModuleProject-result"
    let cacheFilename = cachePath <> show contentHash <> ".json"
    print filename
    result <- withCache cacheFilename (testTypescriptFileInNode filename)
    result `shouldBe` expectedValue

-- | input, output TS, nodeJS output
testCases :: [(Text, Text, String)]
testCases =
  [ ("True", "export const main = true", "true"),
    ("False", "export const main = false", "false"),
    ("123", "export const main = 123", "123"),
    ("\"Poo\"", "export const main = `Poo`", "Poo"),
    ( "if True then 1 else 2",
      "export const main = true ? 1 : 2",
      "1"
    ),
    ( "let a = \"dog\" in 123",
      "const a = `dog`; export const main = 123",
      "123"
    ),
    ( "let a = \"dog\" in let b = \"horse\" in 123",
      "const a = `dog`; \nconst b = `horse`; export const main = 123",
      "123"
    ),
    ( "{ a: 123, b: \"horse\" }",
      "export const main = { a: 123, b: `horse` }",
      "{ a: 123, b: 'horse' }"
    ),
    ("(1,2)", "export const main = [1,2] as const", "[ 1, 2 ]"),
    ("\\a -> let (b,c) = a in b", "export const main = <C,D>(a: readonly [C,D]) => { const [b,c] = a; return b; }", "[Function: main]"),
    ("2 + 2", "export const main = 2 + 2", "4"),
    ("10 - 2", "export const main = 10 - 2", "8"),
    ( "\"dog\" ++ \"log\"",
      "export const main = `dog` + `log`",
      "doglog"
    ),
    ( "{ fun: (\\a -> let d = 1 in a) }",
      "export const main = { fun: <B>(a: B) => { const d = 1; return a; } }",
      "{ fun: [Function: fun] }"
    ),
    ( "[1,2] <> [3,4]",
      "export const main = [...[1,2],...[3,4]]",
      "[ 1, 2, 3, 4 ]"
    ),
    ( "let (a, b) = (1,2) in a",
      "const [a,b] = [1,2] as const; export const main = a",
      "1"
    ),
    ( "let { dog: a, cat: b } = { dog: 1, cat: 2} in (a,b)",
      "const { cat: b, dog: a } = { cat: 2, dog: 1 }; export const main = [a,b] as const",
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
    ( "\\a -> a",
      "[Function: main]"
    ),
    ( "let id a = a; id 1",
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
    ( "let aPair = (1,2); let (a,b) = aPair in a",
      "1"
    ),
    ( "\\a -> let b = 123 in a",
      "[Function: main]"
    ),
    ("(1,2)", "[ 1, 2 ]"),
    ( "Maybe.Just",
      "[Function: Just]"
    ),
    ( "Maybe.Just 1",
      "{ type: 'Just', vars: [ 1 ] }"
    ),
    ( "Maybe.Nothing",
      "{ type: 'Nothing', vars: [] }"
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
    ( "match Maybe.Just True with (Maybe.Just a) -> a | _ -> False",
      "true"
    ),
    ( "match Maybe.Just True with (Maybe.Just a) -> Maybe.Just a | _ -> Maybe.Nothing",
      "{ type: 'Just', vars: [ true ] }"
    ),
    ( "match Maybe.Just True with (Maybe.Just a) -> let b = 1; Maybe.Just a | _ -> Maybe.Nothing",
      "{ type: 'Just', vars: [ true ] }"
    ),
    ( "let (a, b) = (1,2) in a",
      "1"
    ),
    ( "let { dog: a, cat: b } = { dog: 1, cat: 2} in (a,b)",
      "[ 1, 2 ]"
    ),
    ("let str = \"hey\" in match (Maybe.Just str) with (Maybe.Just a) -> a | _ -> \"\"", "hey"),
    ("\"hello world\"", "hello world"),
    ("Either.Right 101", "{ type: 'Right', vars: [ 101 ] }"),
    ("let stringReduce a = 100 in stringReduce", "[Function: main]"),
    ("let const = True; 1", "1"),
    ("2 > 1", "true"),
    ("1 > 2", "false"),
    ("1 >= 1", "true"),
    ("0 >= 1", "false"),
    ("1 < 2", "true"),
    ("2 < 1", "false"),
    ("2 <= 2", "true"),
    ("3 <= 2", "false"),
    ("let and a b = if a then b else False; let a = 1; let b = 3; let c = 6; and False True", "false"),
    ("\"\nHello\n\"", "\nHello\n"),
    ("match Either.Right 1 with Either.Right a -> a | _ -> 0", "1")
  ]

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

    describe "from parsed input" $ do
      traverse_ testIt testCases

      it "simple expression" $ do
        testFromInputText "{ dog: \\a -> a + 100 }"
          `shouldBe` Right "export const main = { dog: (a: number) => a + 100 }"

      -- what the fuck
      it "pattern matching array spreads" $ do
        testFromInputText "\\a -> match a with [a1,...as] -> [as] | [] -> []"
          `shouldBe` Right "export const main = <D>(a: D[]) => { const match = (value: D[]): D[][] => { if (value.length >= 1) { const [a1,...as] = value; return [as]; }; if (value.length === 0) { return []; }; throw new Error(\"Pattern match error\"); }; return match(a); }"

    describe "Entire compilation" $ do
      traverse_ fullTestIt fullTestCases

    describe "Compile `main` function from modules and run them in Node" $ do
      let moduleTestCases =
            [ ( joinLines
                  [ "export def main = 1 + 2"
                  ],
                "3"
              ),
              ( joinLines
                  [ "def adding a b = a + b",
                    "infix +++ = adding",
                    "export def main = 1 +++ 2"
                  ],
                "3"
              ),
              ( joinLines
                  [ "export type Identity a = Identity a",
                    "def runIdentity a = let (Identity inner) = a in inner",
                    "export def main = runIdentity (Identity True)"
                  ],
                "true"
              ),
              ( joinLines
                  [ "export type Either e a = Left e | Right a",
                    "def useEither val = match val with Right a -> a | _ -> False",
                    "def shouldHaveEitherAsDep val = useEither val",
                    "export def main = shouldHaveEitherAsDep (Right True)"
                  ],
                "true"
              )
            ]
       in traverse_ testModule moduleTestCases

    describe "Compile and open entire project" $ do
      xit "Compiles entire project" $ do
        (filename, contentHash) <- testWholeProjectCompile "CompileTSProjectWhole" stdlib Typescript
        cachePath <- createOutputFolder "CompileTSProjectWhole-result"
        let cacheFilename = cachePath <> show contentHash <> ".json"

        result <- withCache cacheFilename (testTypescriptFileInNode filename)
        result `shouldSatisfy` const True
