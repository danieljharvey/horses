{-# LANGUAGE OverloadedStrings #-}

module Test.Interpreter.InterpreterSpec (spec) where

import Data.Foldable (traverse_)
import Data.Text (Text)
import Smol.Core
import Smol.Core.Interpreter.Types.Stack
import Smol.Core.Typecheck.FromParsedExpr
import Test.Helpers
import Test.Hspec

-- | interpret without typechecking etc
doBasicInterpret :: Text -> Expr ResolvedDep ()
doBasicInterpret =
  fmap edAnnotation
    . discardLeft
    . interpret mempty
    . addEmptyStackFrames
    . fromParsedExpr
    . unsafeParseExpr
  where
    discardLeft (Left e) = error (show e)
    discardLeft (Right a) = a

spec :: Spec
spec = do
  describe "InterpreterSpec" $ do
    describe "interpret" $ do
      let cases =
            [ ("1 + 1", "2"),
              ("-11 + 1", "-10"),
              ("(\\a -> a + 1) 41", "42"),
              ("(\\a -> if a then 1 else 2) False", "2"),
              ("(\\a -> if a then 1 else 2) True", "1"),
              ("let a = 41 in a + 1", "42"),
              ("Just (1 + 1)", "Just 2"),
              ("case (Just 1) of Just a -> a + 41 | Nothing -> 0", "42"),
              ("case Nothing of Just a -> a + 41 | Nothing -> 0", "0"),
              ("let stuff = { x: 1, y : 2 }; stuff.x + stuff.y", "3"),
              ("let id = \\a -> a; (id 1, id 2, id 3)", "(1,2,3)"),
              ("[1,2 + 3]", "[1,5]"),
              ("case [1,2,3] of [_, ...rest] -> rest | _ -> [42]", "[2,3]"),
              ("let f = \\a -> if a == 10 then a else a + f (a + 1); f 0", "55")
            ]
      traverse_
        ( \(input, expect) ->
            it (show input <> " = " <> show expect) $ do
              doBasicInterpret input
                `shouldBe` fromParsedExpr (unsafeParseExpr expect)
        )
        cases

    describe "interpret with typeclasses" $ do
      let cases =
            [ ("equals 1 1", "True"), -- use Eq Int
              ("equals (1,1) (1,2)", "False") -- use Eq (a,b) and Eq Int (advanced, not ready for this yet)
            ]
      traverse_
        ( \(input, expect) ->
            it (show input <> " = " <> show expect) $ do
              doBasicInterpret input
                `shouldBe` fromParsedExpr (unsafeParseExpr expect)
        )
        cases
