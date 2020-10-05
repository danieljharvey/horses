{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.BackendJS
  ( spec,
  )
where

import Data.Either (isRight)
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Mimsa.Backend.Backend
import Language.Mimsa.Backend.Javascript
import Language.Mimsa.Printer
import Language.Mimsa.Repl
import Language.Mimsa.Types
import Test.Data.Project
import Test.Hspec

eval :: Project () -> Text -> Either Text Javascript
eval env input =
  case evaluateText env input of
    Left e -> Left $ prettyPrint e
    Right (ResolvedExpression _ storeExpr _ _ _) ->
      pure $
        output (storeExpression storeExpr)

evalModule :: Project () -> Text -> IO (Either Text Javascript)
evalModule env input =
  case evaluateText env input of
    Left e -> pure $ Left $ prettyPrint e
    Right (ResolvedExpression _ storeExpr _ _ _) ->
      let a = outputCommonJS storeExpr
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
    ("Some", "{ type: \"Some\", vars: [] }"),
    ("Some 1", "__app({ type: \"Some\", vars: [] }, 1)"),
    ("case Some 1 of Some \\a -> a | Nowt 0", "__match(__app({ type: \"Some\", vars: [] }, 1), { Some: a => a, Nowt: 0 }, null)"),
    ("case Some 1 of Some \\a -> a | otherwise 0", "__match(__app({ type: \"Some\", vars: [] }, 1), { Some: a => a }, 0)")
  ]

testIt :: (Text, Javascript) -> Spec
testIt (q, a) =
  it (T.unpack q) $
    eval stdLib q `shouldBe` Right a

spec :: Spec
spec =
  describe "JS" $ do
    it "Outputs a module" $ do
      result <- evalModule stdLib "\\a -> compose(id)(id)(a)"
      result `shouldSatisfy` isRight
    traverse_ testIt successes
