{-# LANGUAGE OverloadedStrings #-}

module Test.Transform.AsNewtype
  ( spec,
  )
where

import Data.Map (Map)
import qualified Data.Map as M
import Language.Mimsa.Transform.AsNewtype
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Test.Codegen.Shared
import Test.Hspec
import Test.Utils.Helpers

testDts :: Map TyCon DataType
testDts =
  M.fromList
    [ ("Identity", dtIdentity),
      ("Pair", dtPair),
      ("Right", dtEither),
      ("These", dtThese)
    ]

spec :: Spec
spec = do
  describe "AsNewtype" $ do
    describe "With single constructors" $ do
      it "Does nothing when no pattern match" $ do
        let expr = unsafeParseExpr "let a = True in 1"
        asNewtype testDts expr `shouldBe` expr
      it "Removes a constructor" $ do
        let expr = unsafeParseExpr "Identity a"
            expected = unsafeParseExpr "a"
        asNewtype testDts expr `shouldBe` expected
      it "Simplifies a let pattern" $ do
        let expr = unsafeParseExpr "let (Pair a b) = pair in True"
            expected = unsafeParseExpr "let (a,b) = pair in True"
        asNewtype testDts expr `shouldBe` expected
      it "Simplifies a pattern match (1 arg)" $ do
        let expr = unsafeParseExpr "match identity with (Identity a) -> True"
            expected = unsafeParseExpr "match identity with a -> True"
        asNewtype testDts expr `shouldBe` expected
      it "Simplifies a pattern match (2 arg)" $ do
        let expr = unsafeParseExpr "match pair with (Pair a b) -> True"
            expected = unsafeParseExpr "match pair with (a,b) -> True"
        asNewtype testDts expr `shouldBe` expected
    describe "With multiple constructors" $ do
      it "Does nothing when no pattern match" $ do
        let expr = unsafeParseExpr "let a = True in 1"
        asNewtype testDts expr `shouldBe` expr
      it "Does not removes a constructor" $ do
        let expr = unsafeParseExpr "Right a"
        asNewtype testDts expr `shouldBe` expr
      it "Does not simplify a pattern match (1 arg)" $ do
        let expr = unsafeParseExpr "match either with (Right a) -> True | _ -> False"
        asNewtype testDts expr `shouldBe` expr
      it "Does not simplify a pattern match (2 arg)" $ do
        let expr = unsafeParseExpr "match these with (These a b) -> True | _ -> False"
        asNewtype testDts expr `shouldBe` expr
