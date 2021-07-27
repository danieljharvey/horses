{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Backend.BackendJS
  ( spec,
  )
where

import Data.Bifunctor (first)
import Data.Either (isRight)
import Data.Foldable (traverse_)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Mimsa.Backend.Backend
import Language.Mimsa.Backend.Javascript
import Language.Mimsa.Backend.NormaliseConstructors
import Language.Mimsa.Printer
import Language.Mimsa.Project.Persistence
import Language.Mimsa.Repl
import Language.Mimsa.Store.ResolvedDeps
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store
import Test.Data.Project
import Test.Hspec
import Test.Utils.Helpers

eval :: Project Annotation -> Text -> Either Text Javascript
eval env input =
  case evaluateText env input of
    Left e -> Left $ prettyPrint e
    Right (ResolvedExpression _ storeExpr _ _ _) ->
      first
        prettyPrint
        ( renderWithFunction
            dataTypes
            "main"
            (storeExpression storeExpr)
        )

evalModule :: Project Annotation -> Text -> IO (Either Text Javascript)
evalModule env input =
  case evaluateText env input of
    Left e -> pure $ Left $ prettyPrint e
    Right (ResolvedExpression _ storeExpr _ _ _) -> do
      let a = first prettyPrint (outputCommonJS dataTypes storeExpr)
       in do
            T.putStrLn (prettyPrint a)
            pure a

successes :: [(Text, Javascript)]
successes =
  [ ("True", "const main = true;\n"),
    ("False", "const main = false;\n"),
    ("123", "const main = 123;\n"),
    ("\"Poo\"", "const main = \"Poo\";\n"),
    ("id", "const main = id;\n"),
    ("\\a -> a", "const main = a => a;\n"),
    ("id(1)", "const main = id(1);\n"),
    ("if True then 1 else 2", "const main = true ? 1 : 2;\n"),
    ("let a = \"dog\" in 123", "const main = function() { const a = \"dog\";\nreturn 123 }();\n"),
    ("let a = \"dog\" in let b = \"horse\" in 123", "const main = function() { const a = \"dog\";\nconst b = \"horse\";\nreturn 123 }();\n"),
    ("{ a: 123, b: \"horse\" }", "const main = { a: 123, b: \"horse\" };\n"),
    ("let (a,b) = aPair in a", "const main = function() { const [a, b] = aPair;\nreturn a }();\n"),
    ("\\a -> let b = 123 in a", "const main = a => { const b = 123;\nreturn a };\n"),
    ("(1,2)", "const main = [1,2];\n"),
    ("aRecord.a", "const main = aRecord.a;\n"),
    ("Just", "const main = a => ({ type: \"Just\", vars: [a] });\n"),
    ("Just 1", "const main = { type: \"Just\", vars: [1] };\n"),
    ("Nothing", "const main = { type: \"Nothing\", vars: [] };\n"),
    ("These", "const main = a => b => ({ type: \"These\", vars: [a,b] });\n"),
    ("True == False", "const main = __eq(true, false);\n"),
    ("2 + 2", "const main = 2 + 2;\n"),
    ("10 - 2", "const main = 10 - 2;\n"),
    ("\"dog\" ++ \"log\"", "const main = \"dog\" + \"log\";\n"),
    ("{ fn: (\\a -> let d = 1 in a) }", "const main = { fn: a => { const d = 1;\nreturn a } };\n"),
    ("[1,2] <> [3,4]", "const main = __concat([1, 2], [3, 4]);\n"),
    ( "match Just True with (Just a) -> a | _ -> False",
      "const main = __patternMatch({ type: \"Just\", vars: [true] }, [ [ pat => __eq(pat.type, \"Just\") ? { a: pat.vars[0] } : null, ({ a }) => a ], [ pat => ({}), () => false ] ]);\n"
    ),
    ( "match Just True with (Just a) -> Just a | _ -> Nothing",
      "const main = __patternMatch({ type: \"Just\", vars: [true] }, [ [ pat => __eq(pat.type, \"Just\") ? { a: pat.vars[0] } : null, ({ a }) => ({ type: \"Just\", vars: [a] }) ], [ pat => ({}), () => ({ type: \"Nothing\", vars: [] }) ] ]);\n"
    ),
    ( "let (a, b) = (1,2) in a",
      "const main = function() { const [a, b] = [1,2];\nreturn a }();\n"
    ),
    ( "let { dog: a, cat: b } = { dog: 1, cat: 2} in (a,b)",
      "const main = function() { const { cat: b, dog: a } = { cat: 2, dog: 1 };\nreturn [a,b] }();\n"
    ),
    ( "let (Ident a) = Ident 1 in a",
      "const main = function() { const { vars: [a] } = { type: \"Ident\", vars: [1] };\nreturn a }();\n"
    ),
    ( "let (Pair a b) = Pair 1 2 in (a,b)",
      "const main = function() { const { vars: [a, b] } = { type: \"Pair\", vars: [1,2] };\nreturn [a,b] }();\n"
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

testIt :: (Text, Javascript) -> Spec
testIt (q, a) =
  it (T.unpack q) $
    eval testStdlib q `shouldBe` Right a

dataTypes :: ResolvedTypeDeps Annotation
dataTypes = fromJust $ case resolveTypeDeps
  (prjStore testStdlib)
  (getCurrentTypeBindings $ prjTypeBindings testStdlib) of
  Right a -> Just a
  _ -> Nothing

spec :: Spec
spec = do
  describe "JS" $
    do
      traverse_ testIt successes
      it "Outputs a module" $ do
        result <- evalModule testStdlib "\\a -> compose(id)(id)(a)"
        result `shouldSatisfy` isRight
  describe "Patterns JS" $ do
    let testPattern (q, a) =
          it (T.unpack (prettyPrint q)) $
            outputPattern q `shouldBe` a
    traverse_ testPattern patterns
  describe "Normalise constructors" $ do
    it "is a no-op for nullary constructors" $ do
      let a = MyConstructor mempty "Nothing"
      normaliseConstructors dataTypes a `shouldBe` Right a
    it "turns unary constructor into lambda function" $ do
      let a = MyConstructor mempty "Just"
      let expected =
            MyLambda
              mempty
              "a"
              (MyConsApp mempty (MyConstructor mempty "Just") (MyVar mempty "a"))
      normaliseConstructors dataTypes a `shouldBe` Right expected
    it "turns binary constructor into two lambda functions" $ do
      let a = MyConstructor mempty "These"
      let expected =
            MyLambda
              mempty
              "a"
              ( MyLambda
                  mempty
                  "b"
                  ( MyConsApp
                      mempty
                      ( MyConsApp
                          mempty
                          (MyConstructor mempty "These")
                          (MyVar mempty "a")
                      )
                      (MyVar mempty "b")
                  )
              )
      normaliseConstructors dataTypes a `shouldBe` Right expected
    it "partially applies when wrapped in ConsApp" $ do
      let a = MyConsApp mempty (MyConstructor mempty "These") (int 1)
      let expected =
            MyLambda
              mempty
              "b"
              ( MyConsApp
                  mempty
                  ( MyConsApp
                      mempty
                      (MyConstructor mempty "These")
                      (int 1)
                  )
                  (MyVar mempty "b")
              )
      normaliseConstructors dataTypes a `shouldBe` Right expected
    it "completely applies when wrapped in ConsApp" $ do
      let a =
            MyConsApp
              mempty
              ( MyConsApp
                  mempty
                  (MyConstructor mempty "These")
                  (int 1)
              )
              (int 2)
      normaliseConstructors dataTypes a `shouldBe` Right a
