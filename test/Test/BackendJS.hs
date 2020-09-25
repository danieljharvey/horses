{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.BackendJS
  ( spec,
  )
where

import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Backend.Javascript
import Language.Mimsa.Printer
import Language.Mimsa.Repl
import Language.Mimsa.Types
import Test.Data.Project
import Test.Hspec

eval :: Project -> Text -> IO (Either Text Javascript)
eval env input =
  case evaluateText env input of
    Left e -> pure (Left $ prettyPrint e)
    Right (ResolvedExpression _ storeExpr _ _ _) ->
      pure $ Right $
        output (storeExpression storeExpr)

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
testIt (q, a) = it (T.unpack q) $ do
  result <- eval stdLib q
  result `shouldBe` Right a

spec :: Spec
spec =
  describe "JS" $
    traverse_ testIt successes
