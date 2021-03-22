{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Backend.BackendJS
  ( spec,
  )
where

import Data.Bifunctor (first)
import Data.Either (isRight)
import Data.Foldable (traverse_)
import Data.Functor (($>))
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
import Language.Mimsa.Types.Identifiers ()
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
    ("let (a,b) = aPair in a", "const main = function() { const [a,b] = aPair;\nreturn a }();\n"),
    ("\\a -> let b = 123 in a", "const main = a => { const b = 123;\nreturn a };\n"),
    ("(1,2)", "const main = [1,2];\n"),
    ("aRecord.a", "const main = aRecord.a;\n"),
    ("Some", "const main = a => ({ type: \"Some\", vars: [a] });\n"),
    ("Some 1", "const main = { type: \"Some\", vars: [1] };\n"),
    ("Nowt", "const main = { type: \"Nowt\", vars: [] };\n"),
    ("These", "const main = a => b => ({ type: \"These\", vars: [a,b] });\n"),
    ("case Some 1 of Some \\a -> a | Nowt 0", "const main = __match({ type: \"Some\", vars: [1] }, { Some: a => a, Nowt: 0 }, null);\n"),
    ("case Some 1 of Some \\a -> a | otherwise 0", "const main = __match({ type: \"Some\", vars: [1] }, { Some: a => a }, 0);\n"),
    ("True == False", "const main = __eq(true, false);\n"),
    ("2 + 2", "const main = 2 + 2;\n"),
    ("10 - 2", "const main = 10 - 2;\n"),
    ("\"dog\" <> \"log\"", "const main = \"dog\" + \"log\";\n"),
    ("{ fn: (\\a -> let d = 1 in a) }", "const main = { fn: a => { const d = 1;\nreturn a } };\n")
  ]

testIt :: (Text, Javascript) -> Spec
testIt (q, a) =
  it (T.unpack q) $
    eval stdLib q `shouldBe` Right a

dataTypes :: ResolvedTypeDeps Annotation
dataTypes = fromJust $ case resolveTypeDeps
  (prjStore stdLib)
  (getCurrentTypeBindings $ prjTypeBindings stdLib) of
  Right a -> Just a
  _ -> Nothing

spec :: Spec
spec = do
  describe "JS" $
    do
      traverse_ testIt successes
      it "Outputs a module" $ do
        result <- evalModule stdLib "\\a -> compose(id)(id)(a)"
        result `shouldSatisfy` isRight
  describe "Normalise constructors" $ do
    it "is a no-op for nullary constructors" $ do
      let a = MyConstructor mempty "Nowt"
      normaliseConstructors dataTypes a `shouldBe` Right a
    it "turns unary constructor into lambda function" $ do
      let a = MyConstructor mempty "Some"
      let expected =
            MyLambda
              mempty
              "a"
              (MyConsApp mempty (MyConstructor mempty "Some") (MyVar mempty "a"))
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
