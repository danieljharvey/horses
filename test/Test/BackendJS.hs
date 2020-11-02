{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.BackendJS
  ( spec,
  )
where

import Data.Either (isRight)
import Data.Foldable (traverse_)
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
import Test.Helpers
import Test.Hspec

eval :: Project Annotation -> Text -> Either Text Javascript
eval env input =
  case evaluateText env input of
    Left e -> Left $ prettyPrint e
    Right (ResolvedExpression _ storeExpr _ _ _) ->
      pure $
        output dataTypes (storeExpression storeExpr)

evalModule :: Project Annotation -> Text -> IO (Either Text Javascript)
evalModule env input =
  case evaluateText env input of
    Left e -> pure $ Left $ prettyPrint e
    Right (ResolvedExpression _ storeExpr _ _ _) ->
      let a = outputCommonJS dataTypes storeExpr
       in do
            T.putStrLn (prettyPrint a)
            pure (Right a)

successes :: [(Text, Javascript)]
successes =
  [ ("True", "true"),
    ("False", "false"),
    ("123", "123"),
    ("\"Poo\"", "\"Poo\""),
    ("id", "id"),
    ("\\a -> a", "a => a"),
    ("id(1)", "id(1)"),
    ("if True then 1 else 2", "true ? 1 : 2"),
    ("let a = \"dog\" in 123", "const a = \"dog\";\nreturn 123"),
    ("let a = \"dog\" in let b = \"horse\" in 123", "const a = \"dog\";\nconst b = \"horse\";\nreturn 123"),
    ("{ a: 123, b: \"horse\" }", "{ a: 123, b: \"horse\" }"),
    ("let (a,b) = aPair in a", "const [a,b] = aPair;\nreturn a"),
    ("\\a -> let b = 123 in a", "a => { const b = 123;\nreturn a }"),
    ("(1,2)", "[1,2]"),
    ("aRecord.a", "aRecord.a"),
    ("Some", "a => ({ type: \"Some\", vars: [a] })"),
    ("Some 1", "{ type: \"Some\", vars: [1] }"),
    ("Nowt", "{ type: \"Nowt\", vars: [] }"),
    ("These", "a => b => ({ type: \"These\", vars: [a,b] })"),
    ("case Some 1 of Some \\a -> a | Nowt 0", "__match({ type: \"Some\", vars: [1] }, { Some: a => a, Nowt: 0 }, null)"),
    ("case Some 1 of Some \\a -> a | otherwise 0", "__match({ type: \"Some\", vars: [1] }, { Some: a => a }, 0)"),
    ("True == False", "__eq(true, false)"),
    ("2 + 2", "2 + 2"),
    ("10 - 2", "10 - 2")
  ]

testIt :: (Text, Javascript) -> Spec
testIt (q, a) =
  it (T.unpack q) $
    eval stdLib q `shouldBe` Right a

dataTypes :: ResolvedTypeDeps
dataTypes = fromJust $ case resolveTypeDeps
  (store stdLib)
  (getCurrentTypeBindings $ typeBindings stdLib) of
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
      let a = MyConstructor () (mkTyCon "Nowt")
      normaliseConstructors dataTypes a `shouldBe` a
    it "turns unary constructor into lambda function" $ do
      let a = MyConstructor () (mkTyCon "Some")
      let expected =
            MyLambda
              mempty
              (mkName "a")
              (MyConsApp mempty (MyConstructor mempty (mkTyCon "Some")) (MyVar mempty (mkName "a")))
      normaliseConstructors dataTypes a `shouldBe` expected
    it "turns binary constructor into two lambda functions" $ do
      let a = MyConstructor () (mkTyCon "These")
      let expected =
            MyLambda
              mempty
              (mkName "a")
              ( MyLambda
                  mempty
                  (mkName "b")
                  ( MyConsApp
                      mempty
                      ( MyConsApp
                          mempty
                          (MyConstructor mempty (mkTyCon "These"))
                          (MyVar mempty (mkName "a"))
                      )
                      (MyVar mempty (mkName "b"))
                  )
              )
      normaliseConstructors dataTypes a `shouldBe` expected
    it "partially applies when wrapped in ConsApp" $ do
      let a = MyConsApp () (MyConstructor mempty (mkTyCon "These")) (int 1)
      let expected =
            MyLambda
              mempty
              (mkName "b")
              ( MyConsApp
                  mempty
                  ( MyConsApp
                      mempty
                      (MyConstructor mempty (mkTyCon "These"))
                      (int 1)
                  )
                  (MyVar mempty (mkName "b"))
              )
      normaliseConstructors dataTypes a `shouldBe` expected
    it "completely applies when wrapped in ConsApp" $ do
      let a =
            MyConsApp
              ()
              ( MyConsApp
                  mempty
                  (MyConstructor mempty (mkTyCon "These"))
                  (int 1)
              )
              (int 2)
      normaliseConstructors dataTypes a `shouldBe` a
