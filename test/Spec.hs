{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

-- import qualified Data.Aeson as JSON
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa
import Test.Hspec
import qualified Test.Interpreter as Interpreter
import Test.QuickCheck.Instances ()
import qualified Test.Resolver as Resolver
import qualified Test.Substitutor as Substitutor
import qualified Test.Syntax as Syntax

charListToText :: [Char] -> Text
charListToText = foldr T.cons ""

exprs :: [(Expr, Either Text MonoType)]
exprs =
  [ (MyInt 1, Right MTInt),
    (MyBool True, Right MTBool),
    ( MyString
        (StringType "hello"),
      Right MTString
    ),
    (MyVar (mkName "x"), Left "Unknown variable \"x\""),
    (MyLet (mkName "x") (MyInt 42) (MyBool True), Right MTBool),
    (MyLet (mkName "x") (MyInt 42) (MyVar (mkName "x")), Right MTInt),
    ( MyLet
        (mkName "x")
        (MyBool True)
        (MyLet (mkName "y") (MyInt 42) (MyVar (mkName "x"))),
      Right MTBool
    ),
    ( MyLet
        (mkName "x")
        (MyBool True)
        (MyLet (mkName "x") (MyInt 42) (MyVar (mkName "x"))),
      Right MTInt
    ),
    ( MyLambda (mkName "x") (MyBool True),
      Right $ MTFunction (MTUnknown (UniVar 1)) MTBool
    ),
    ( identity,
      Right $ MTFunction (MTUnknown (UniVar 1)) (MTUnknown (UniVar 1))
    ),
    ( MyLambda (mkName "x") (MyLambda (mkName "y") (MyVar (mkName "x"))),
      Right $
        MTFunction
          (MTUnknown (UniVar 1))
          (MTFunction (MTUnknown (UniVar 2)) (MTUnknown (UniVar 1)))
    ),
    ( MyApp
        ( MyLambda
            (mkName "x")
            (MyBool True)
        )
        (MyInt 1),
      Right MTBool
    ),
    ( MyApp
        identity
        (MyInt 1),
      Right MTInt
    ),
    ( MyApp
        ( MyLambda
            (mkName "x")
            ( (MyIf (MyVar (mkName "x")) (MyInt 10) (MyInt 10))
            )
        )
        (MyInt 100),
      Left "Can't match MTBool with MTInt"
    ),
    ( MyLambda (mkName "x") (MyApp (MyVar (mkName "x")) (MyVar (mkName "x"))),
      Left "Cannot unify as MTUnknown 1 occurs within MTFunction (MTUnknown 1) (MTUnknown 2)"
    )
  ]

identity :: Expr
identity = (MyLambda (mkName "x") (MyVar (mkName "x")))

main :: IO ()
main = hspec $ do
  Syntax.spec
  Interpreter.spec
  Resolver.spec
  Substitutor.spec
  describe "Typechecker" $ do
    it "Our expressions typecheck as expected" $ do
      _ <- traverse (\(code, expected) -> startInference code `shouldBe` expected) exprs
      pure ()
    it "We can use identity with two different datatypes in one expression" $ do
      let lambda = (MyLambda (mkName "x") (MyIf (MyApp identity (MyVar (mkName "x"))) (MyApp identity (MyInt 1)) (MyApp identity (MyInt 2))))
      let expr = MyApp lambda (MyBool True)
      (startInference lambda) `shouldBe` Right (MTFunction (MTUnknown 1) MTInt)
      (startInference expr) `shouldBe` Right MTInt
{-  describe "Serialisation" $ do
it "Round trip" $ do
  property $ \x -> JSON.decode (JSON.encode x) == (Just x :: Maybe Expr)
-}
