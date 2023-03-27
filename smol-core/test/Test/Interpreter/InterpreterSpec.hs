{-# LANGUAGE OverloadedStrings #-}

module Test.Interpreter.InterpreterSpec (spec) where

import Control.Monad.Reader
import Data.Foldable (traverse_)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import Smol.Core
import Smol.Core.IR.FromResolvedExpr
import Smol.Core.Typecheck.FromParsedExpr
import Test.Helpers
import Test.Hspec

toIdentity :: ParsedExpr ann -> IdentityExpr ann
toIdentity = fromResolvedExpr . fromParsedExpr

doInterpret :: Text -> IdentityExpr ()
doInterpret =
  toExpr
    . flip runReader emptyEnv
    . interpret
    . fromExpr
    . toIdentity
    . unsafeParseExpr

spec :: Spec
spec = do
  describe "InterpreterSpec" $ do
    describe "interpret" $ do
      let cases =
            [ ("a", "a"),
              ("1 + 1", "2"),
              ("-11 + 1", "-10"),
              ("(\\a -> a + 1) 41", "42"),
              ("(\\a -> if a then 1 else 2) False", "2"),
              ("(\\a -> if a then 1 else 2) True", "1"),
              ("let a = 41 in a + 1", "42"),
              ("Just (1 + 1)", "Just 2"),
              ("case (Just 1) of Just a -> a + 41 | Nothing -> 0", "42"),
              ("case Nothing of Just a -> a + 41 | Nothing -> 0", "0"),
              ("let stuff = { x: 1, y : 2 }; stuff.x + stuff.y", "3"),
              ("global dog = 100; dog! + 1", "101"),
              ("global dog = 100; global dog = 200; dog!", "200"),
              ("let id = \\a -> a; (id 1, id 2, id 3)", "(1,2,3)")
            ]
      traverse_
        ( \(input, expect) ->
            it (show input <> " = " <> show expect) $ do
              doInterpret input
                `shouldBe` toIdentity (unsafeParseExpr expect)
        )
        cases

    describe "toExpr" $ do
      it "Converts from lambda" $ do
        let from = ILambda () "a" id
            to = ELambda () "a" (EVar () "a")
        toExpr from `shouldBe` to
      it "Converts from nested lambda" $ do
        let from = ILambda () "a" (ILambda () "b" . const)
            to = ELambda () "a" (ELambda () "b" (EVar () "a"))
        toExpr from `shouldBe` to
      it "Converts pattern match" $ do
        let from = IPatternMatch () (IVar () "a") (NE.fromList [(PVar () "b", \env -> IRecordAccess () env "b")])
            to = toIdentity $ unsafeParseExpr "case a of b -> b"
        toExpr from `shouldBe` to

    describe "fromExpr" $ do
      it "Converts to lambda" $ do
        let from = ELambda () "a" (EVar () "a")
        (toExpr . fromExpr) from `shouldBe` from
      it "Converts to nested lambda" $ do
        let from = ELambda () "a" (ELambda () "b" (EVar () "a"))
        (toExpr . fromExpr) from `shouldBe` from
      it "Converts let to lambda" $ do
        let from = toIdentity $ unsafeParseExpr "let a = 41 in a + 1"
        (toExpr . fromExpr) from `shouldBe` from
      it "Converts pattern matches" $ do
        let from = toIdentity $ unsafeParseExpr "case Just 1 of Just a -> a | Nothing -> 0"
        (toExpr . fromExpr) from `shouldBe` from