{-# LANGUAGE OverloadedStrings #-}

module Test.Backend.ESModulesJS
  ( spec,
  )
where

import Control.Monad.Except
import Data.Bifunctor
import Data.Foldable
import Data.Functor
import Data.Hashable
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.Mimsa.Backend.Javascript.Printer as JS
import Language.Mimsa.Backend.Types
import Language.Mimsa.Backend.Typescript.DataType
import Language.Mimsa.Backend.Typescript.FromExpr
import Language.Mimsa.Backend.Typescript.Monad
import Language.Mimsa.Backend.Typescript.Patterns
import Language.Mimsa.Backend.Typescript.Types
import Language.Mimsa.Printer
import Language.Mimsa.Project.Stdlib
import Language.Mimsa.Types.AST
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
  let readerState = TSReaderState mempty mempty
      startState = TSCodegenState mempty mempty mempty
   in case fromExpr readerState startState expr of
        Right (ejsModule, _) -> (ejsModule, JS.printModule ejsModule)
        Left e -> error (T.unpack (prettyPrint e))

testFromInputText :: Text -> Either Text Text
testFromInputText input =
  case evaluateText testStdlib input of
    Left e -> throwError (prettyPrint e)
    Right resolved -> do
      let exprName = first fst (reTypedExpression resolved)
      let readerState = TSReaderState mempty mempty
          startState = TSCodegenState mempty mempty mempty
      first prettyPrint (JS.printModule . fst <$> fromExpr readerState startState exprName)

-- test that we have a valid ESModulesJS module by saving it and running it
testESModulesJSInNode :: Text -> IO String
testESModulesJSInNode ts = do
  -- write file
  tsPath <- createOutputFolder "ESModulesJS"
  let tsFilename = tsPath <> show (hash ts) <> ".mjs"
  -- cache output
  cachePath <- createOutputFolder "ESModulesJS-result"
  let cacheFilename = cachePath <> show (hash ts) <> ".json"
  -- create output
  let tsOutput = ts <> "\nconsole.log(main)"
  writeFile tsFilename (T.unpack tsOutput)
  (ec, err) <- withCache cacheFilename (runScriptFromFile tsFilename)
  if ec then pure err else fail err

-- test that we have a valid ESModulesJS module by saving it and running it
testESModulesJSFileInNode :: FilePath -> IO String
testESModulesJSFileInNode tsFilename = do
  -- create output
  (ec, err) <- runScriptFromFile tsFilename
  if ec then pure err else fail err

testIt :: (Text, Text, String) -> Spec
testIt (expr, expectedTS, expectedValue) =
  it (T.unpack expr) $ do
    case testFromInputText expr of
      Left e -> fail (T.unpack e)
      Right ts -> do
        ts `shouldBe` expectedTS
        val <- testESModulesJSInNode ts
        val `shouldBe` expectedValue

fullTestIt :: (Text, String) -> Spec
fullTestIt (input, expectedValue) =
  it (T.unpack input) $ do
    let unsafeParse = ($> mempty) . unsafeParseExpr
        expr = unsafeParse input
    (filename, contentHash) <- testProjectCompile "CompileJSProject" ESModulesJS expr
    cachePath <- createOutputFolder "CompileJSProject-result"
    let cacheFilename = cachePath <> show contentHash <> ".json"

    result <- withCache cacheFilename (testESModulesJSFileInNode filename)
    result `shouldBe` expectedValue

testModule :: (Text, String) -> Spec
testModule (input, expectedValue) =
  it (T.unpack input) $ do
    (filename, contentHash) <- testModuleCompile "CompileJSModuleProject" ESModulesJS input
    cachePath <- createOutputFolder "CompileJSModuleProject-result"
    let cacheFilename = cachePath <> show contentHash <> ".json"

    result <- withCache cacheFilename (testESModulesJSFileInNode filename)
    result `shouldBe` expectedValue

-- | input, output ejs, nodeJS output
testCases :: [(Text, Text, String)]
testCases =
  [ ("True", "export const main = true", "true"),
    ("False", "export const main = false", "false"),
    ("123", "export const main = 123", "123"),
    ("\"Poo\"", "export const main = `Poo`", "Poo"),
    ( "\\a -> a",
      "export const main = (a) => a",
      "[Function: main]"
    ),
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
    ( "\\a -> let b = 123 in a",
      "export const main = (a) => { const b = 123; return a; }",
      "[Function: main]"
    ),
    ("(1,2)", "export const main = [1,2]", "[ 1, 2 ]"),
    ("2 + 2", "export const main = 2 + 2", "4"),
    ("10 - 2", "export const main = 10 - 2", "8"),
    ( "\"dog\" ++ \"log\"",
      "export const main = `dog` + `log`",
      "doglog"
    ),
    ( "{ fn: (\\a -> let d = 1 in a) }",
      "export const main = { fn: (a) => { const d = 1; return a; } }",
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
    ("id", "[Function: main]"),
    ( "\\a -> a",
      "[Function: main]"
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
      "[Function: main]"
    ),
    ("(1,2)", "[ 1, 2 ]"),
    ("aRecord.a", "1"),
    ( "Just",
      "[Function: Just]"
    ),
    ( "Just 1",
      "{ type: 'Just', vars: [ 1 ] }"
    ),
    ( "Nothing",
      "{ type: 'Nothing', vars: [] }"
    ),
    ( "These",
      "[Function: These]"
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
    ),
    ( "type Id a = Id a; let (Id aaa) = Id \"dog\" in aaa",
      "dog"
    ),
    ("let str = \"hey\" in match (Just str) with (Just a) -> a | _ -> \"\"", "hey"),
    ("\"hello world\"", "hello world"),
    ("id \"hello again\"", "hello again"),
    ( "either.fmap (\\a -> a + 1) (Right 100)",
      "{ type: 'Right', vars: [ 101 ] }"
    ),
    ( "let fold total item = total <> [item] in stringReduce fold [] \"dog\"",
      "[ 'd', 'o', 'g' ]"
    ),
    ("let const = True; 1", "1"),
    ("2 > 1", "true"),
    ("1 > 2", "false"),
    ("1 >= 1", "true"),
    ("0 >= 1", "false"),
    ("1 < 2", "true"),
    ("2 < 1", "false"),
    ("2 <= 2", "true"),
    ("3 <= 2", "false"),
    ("\"\nHello\n\"", "\nHello\n")
  ]

spec :: Spec
spec = do
  xdescribe "ESModulesJS" $ do
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
            `shouldBe` "const d = value.charAt(0); const og = value.slice(1); "
          destructure' (TSPatternConstructor "Just" [TSPatternString (TSStringVar "d") (TSStringVar "og")])
            `shouldBe` "const d = value.vars[0].charAt(0); const og = value.vars[0].slice(1); "

        it "conditions" $ do
          let conditions' = JS.printExpr . conditions
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
                  (MTPair mempty mtBool mtBool)
                  (PPair (MTPair mempty mtBool mtBool) (PVar mtBool "a") (PWildcard mtBool))
                  ( MyPair
                      (MTPair mempty mtBool mtBool)
                      (MyLiteral mtBool (MyBool True))
                      (MyLiteral mtBool (MyBool False))
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

    describe "from parsed input" $ do
      traverse_ testIt testCases

      it "simple expression" $ do
        testFromInputText "\\a -> a + 100"
          `shouldBe` Right "export const main = (a) => a + 100"

      it "pattern matching array spreads" $ do
        testFromInputText "\\a -> match a with [a1,...as] -> [as] | [] -> []"
          `shouldBe` Right "export const main = (a) => { const match = (value) => { if (value.length >= 1) { const [a1,...as] = value; return [as]; }; if (value.length === 0) { return []; }; throw new Error(\"Pattern match error\"); }; return match(a); }"

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

    -- Unique variable errors here, maybe more than we want to chew off rn?
    describe "Compile and open entire project" $ do
      xit "Compiles entire project" $ do
        (filename, contentHash) <- testWholeProjectCompile "CompileJSProjectWhole" stdlib ESModulesJS
        cachePath <- createOutputFolder "CompileJSProjectWhole-result"
        let cacheFilename = cachePath <> show contentHash <> ".json"

        result <- withCache cacheFilename (testESModulesJSFileInNode filename)
        result `shouldSatisfy` const True
