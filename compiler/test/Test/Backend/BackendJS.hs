{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Backend.BackendJS
  ( spec,
  )
where

import Data.Bifunctor (first)
import Data.Coerce
import Data.Either (isRight)
import Data.Foldable (traverse_)
import Data.Hashable
import Data.List (intersperse)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Language.Mimsa.Actions.Shared as Actions
import Language.Mimsa.Backend.Backend
import Language.Mimsa.Backend.Javascript
import Language.Mimsa.Backend.NormaliseConstructors
import Language.Mimsa.Printer
import Language.Mimsa.Project.Persistence
import Language.Mimsa.Store.ResolvedDeps
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store
import Test.Backend.RunNode (lbsToString, runScriptFromFile, withCache)
import Test.Data.Project
import Test.Hspec
import Test.Utils.Helpers
import Test.Utils.Serialisation
  ( createOutputFolder,
  )

deriving newtype instance Hashable Javascript

eval :: Backend -> Project Annotation -> Text -> Either Text Javascript
eval be env input =
  case Actions.evaluateText env input of
    Left e -> Left $ prettyPrint e
    Right resolvedExpr ->
      first
        prettyPrint
        ( renderWithFunction
            be
            dataTypes
            "main"
            (storeExpression (reStoreExpression resolvedExpr))
        )

evalModule :: Project Annotation -> Text -> IO (Either Text Javascript)
evalModule env input =
  case Actions.evaluateText env input of
    Left e -> pure $ Left $ prettyPrint e
    Right resolvedExpr -> do
      let mt = reMonoType resolvedExpr
          storeExpr = reStoreExpression resolvedExpr
          a = first prettyPrint (outputJavascript CommonJS dataTypes mt storeExpr)
       in do
            T.putStrLn (prettyPrint a)
            pure a

successes :: [(Text, Javascript, String)]
successes =
  [ ("True", "const main = true;\n", "true"),
    ("False", "const main = false;\n", "false"),
    ("123", "const main = 123;\n", "123"),
    ("\"Poo\"", "const main = \"Poo\";\n", "Poo"),
    ("id", "const main = id;\n", "[Function: id]"),
    ( "\\a -> a",
      "const main = a => a;\n",
      "[Function: main]"
    ),
    ( "id 1",
      "const main = id(1);\n",
      "1"
    ),
    ( "if True then 1 else 2",
      "const main = true ? 1 : 2;\n",
      "1"
    ),
    ( "let a = \"dog\" in 123",
      "const main = function() { const a = \"dog\";\nreturn 123 }();\n",
      "123"
    ),
    ( "let a = \"dog\" in let b = \"horse\" in 123",
      "const main = function() { const a = \"dog\";\nconst b = \"horse\";\nreturn 123 }();\n",
      "123"
    ),
    ( "{ a: 123, b: \"horse\" }",
      "const main = { a: 123, b: \"horse\" };\n",
      "{ a: 123, b: 'horse' }"
    ),
    ( "let (a,b) = aPair in a",
      "const main = function() { const [a, b] = aPair;\nreturn a }();\n",
      "1"
    ),
    ( "\\a -> let b = 123 in a",
      "const main = a => { const b = 123;\nreturn a };\n",
      "[Function: main]"
    ),
    ("(1,2)", "const main = [1,2];\n", "[ 1, 2 ]"),
    ("aRecord.a", "const main = aRecord.a;\n", "100"),
    ( "Just",
      "const main = a => ({ type: \"Just\", vars: [a] });\n",
      "[Function: main]"
    ),
    ( "Just 1",
      "const main = { type: \"Just\", vars: [1] };\n",
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

patterns :: [(Pattern Name (), Javascript)]
patterns =
  [ (PWildcard mempty, "pat => ({})"),
    (PVar mempty "a", "pat => ({ a: pat })"),
    (PVar mempty "b", "pat => ({ b: pat })"),
    ( PPair mempty (PVar mempty "a") (PVar mempty "b"),
      "pat => ({ a: pat[0], b: pat[1] })"
    ),
    ( PLit mempty (MyBool True),
      "pat => __eq(pat, true) ? {} : null"
    ),
    ( PPair
        mempty
        (PLit mempty (MyBool True))
        (PLit mempty (MyBool False)),
      "pat => __eq(pat[0], true) && __eq(pat[1], false) ? {} : null"
    ),
    ( PRecord
        mempty
        ( M.fromList
            [ ("dog", PVar mempty "a"),
              ("cat", PLit mempty (MyInt 100))
            ]
        ),
      "pat => __eq(pat.cat, 100) ? { a: pat.dog } : null"
    ),
    ( PConstructor mempty "Just" [PVar mempty "a"],
      "pat => __eq(pat.type, \"Just\") ? { a: pat.vars[0] } : null"
    ),
    (PArray mempty [] NoSpread, "pat => pat.length === 0 ? {} : null"),
    ( PArray mempty [PVar mempty "a", PLit mempty (MyBool True)] NoSpread,
      "pat => pat.length === 2 && __eq(pat[1], true) ? { a: pat[0] } : null"
    ),
    ( PArray mempty [PVar mempty "a", PLit mempty (MyBool True)] (SpreadWildcard mempty),
      "pat => pat.length >= 2 && __eq(pat[1], true) ? { a: pat[0] } : null"
    ),
    ( PArray mempty [PVar mempty "a", PLit mempty (MyBool True)] (SpreadValue mempty "b"),
      "pat => pat.length >= 2 && __eq(pat[1], true) ? { a: pat[0], b: pat.slice(2) } : null"
    ),
    ( PString mempty (StrWildcard mempty) (StrWildcard mempty),
      "pat => pat.length >= 1 ? {} : null"
    ),
    ( PString mempty (StrValue mempty "a") (StrValue mempty "as"),
      "pat => pat.length >= 1 ? { a: pat.charAt(0), as: pat.slice(1) } : null"
    )
  ]

commonJSImports :: String
commonJSImports =
  "const { __eq, __concat, __patternMatch } = require('../../../static/backend/commonjs/stdlib.js')"

esModulesImports :: String
esModulesImports =
  "import { __eq, __concat, __patternMatch } from '../../../static/backend/es-modules-js/stdlib.mjs'"

testJSCode :: [String]
testJSCode =
  [ "const id = a => a",
    "const aRecord = {a:100}",
    "const aPair = [1,2]"
  ]

createCommonJSOutput :: String -> String
createCommonJSOutput js =
  mconcat $
    intersperse
      "\n"
      ( [commonJSImports]
          <> testJSCode
          <> [js, "console.log(main)"]
      )

createESModulesOutput :: String -> String
createESModulesOutput js =
  mconcat $
    intersperse
      "\n"
      ( [esModulesImports]
          <> testJSCode
          <> [ js,
               "console.log(main)"
             ]
      )

-- test that we have a valid ESModule by saving it and running it
testESModulesInNode :: Javascript -> IO String
testESModulesInNode js = do
  -- write file
  esPath <- createOutputFolder "ESModules"
  let esFilename = esPath <> show (hash js) <> ".mjs"

  cachePath <- createOutputFolder "ESModules-result"
  let cacheFilename = cachePath <> show (hash js) <> ".json"

  let jsString = createESModulesOutput (lbsToString $ coerce js)
  writeFile esFilename jsString
  (ec, err) <- withCache cacheFilename (runScriptFromFile esFilename)
  if ec then pure err else fail err

-- test that we have a valid ESModule by saving it and running it
testCommonJSInNode :: Javascript -> IO String
testCommonJSInNode js = do
  -- write file
  cjsPath <- createOutputFolder "CommonJS"
  let cjsFilename = cjsPath <> show (hash js) <> ".js"

  cachePath <- createOutputFolder "CommonJS-result"
  let cacheFilename = cachePath <> show (hash js) <> ".json"

  let jsString = createCommonJSOutput (lbsToString $ coerce js)
  writeFile cjsFilename jsString

  (ec, err) <- withCache cacheFilename (runScriptFromFile cjsFilename)
  if ec then pure err else fail err

testIt :: (Text, Javascript, String) -> Spec
testIt (expr, expectedJS, expectedValue) =
  it (T.unpack expr) $ do
    case eval CommonJS testStdlib expr of
      Left e -> fail (T.unpack e)
      Right js -> do
        js `shouldBe` expectedJS
        val <- testCommonJSInNode js
        val `shouldBe` expectedValue

    case eval ESModulesJS testStdlib expr of
      Left e -> fail (T.unpack e)
      Right js -> do
        val <- testESModulesInNode js
        val `shouldBe` expectedValue

dataTypes :: ResolvedTypeDeps
dataTypes = fromJust $ case resolveTypeDeps
  (prjStore testStdlib)
  (getCurrentTypeBindings $ prjTypeBindings testStdlib) of
  Right a -> Just a
  _ -> Nothing

spec :: Spec
spec = do
  describe "Backend JS" $ do
    describe "JS" $
      do
        traverse_ testIt successes
    describe "stuff" $ do
      it "Outputs a module" $ do
        result <- evalModule testStdlib "\\a -> (compose id id) a"
        result `shouldSatisfy` isRight
    describe "Patterns JS" $ do
      let testPattern (q, a) =
            it (T.unpack (prettyPrint q)) $
              outputPattern q `shouldBe` a
      traverse_ testPattern patterns
    describe "Normalise constructors" $ do
      it "is a no-op for nullary constructors" $ do
        let a = MyConstructor () "Nothing"
        normaliseConstructors dataTypes a `shouldBe` Right a
      it "turns unary constructor into lambda function" $ do
        let a = MyConstructor () "Just"
        let expected =
              MyLambda
                mempty
                "a"
                (MyApp mempty (MyConstructor mempty "Just") (MyVar mempty "a"))
        normaliseConstructors dataTypes a `shouldBe` Right expected
      it "turns binary constructor into two lambda functions" $ do
        let a = MyConstructor () "These"
        let expected =
              MyLambda
                mempty
                "a"
                ( MyLambda
                    mempty
                    "b"
                    ( MyApp
                        mempty
                        ( MyApp
                            mempty
                            (MyConstructor mempty "These")
                            (MyVar mempty "a")
                        )
                        (MyVar mempty "b")
                    )
                )
        normaliseConstructors dataTypes a `shouldBe` Right expected
      it "partially applies when wrapped in ConsApp" $ do
        let a = MyApp () (MyConstructor () "These") (int 1)
        let expected =
              MyLambda
                mempty
                "b"
                ( MyApp
                    mempty
                    ( MyApp
                        mempty
                        (MyConstructor mempty "These")
                        (int 1)
                    )
                    (MyVar mempty "b")
                )
        normaliseConstructors dataTypes a `shouldBe` Right expected
      it "completely applies when wrapped in ConsApp" $ do
        let a =
              MyApp
                ()
                ( MyApp
                    ()
                    (MyConstructor () "These")
                    (int 1)
                )
                (int 2)
        normaliseConstructors dataTypes a `shouldBe` Right a
