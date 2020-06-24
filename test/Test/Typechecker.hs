{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Typechecker
  ( spec,
  )
where

-- import qualified Data.Aeson as JSON
import Data.Text (Text)
import Language.Mimsa
import Test.Helpers
import Test.Hspec
import Test.QuickCheck.Instances ()

exprs :: [(Expr, Either Text MonoType)]
exprs =
  [ (int 1, Right MTInt),
    (bool True, Right MTBool),
    ( str
        (StringType "hello"),
      Right MTString
    ),
    -- (MyVar (mkName "x"), Left "Unknown variable \"x\""),
    (MyLet (mkName "x") (int 42) (bool True), Right MTBool),
    (MyLet (mkName "x") (int 42) (MyVar (mkName "x")), Right MTInt),
    ( MyLet
        (mkName "x")
        (bool True)
        (MyLet (mkName "y") (int 42) (MyVar (mkName "x"))),
      Right MTBool
    ),
    ( MyLet
        (mkName "x")
        (bool True)
        (MyLet (mkName "x") (int 42) (MyVar (mkName "x"))),
      Right MTInt
    ),
    ( MyLambda (mkName "x") (bool True),
      Right $ MTFunction (unknown 1) MTBool
    ),
    ( identity,
      Right $ MTFunction (unknown 1) (unknown 1)
    ),
    ( MyLambda (mkName "x") (MyLambda (mkName "y") (MyVar (mkName "x"))),
      Right $
        MTFunction
          (unknown 1)
          (MTFunction (unknown 2) (unknown 1))
    ),
    ( MyApp
        ( MyLambda
            (mkName "x")
            (bool True)
        )
        (int 1),
      Right MTBool
    ),
    ( MyApp
        identity
        (int 1),
      Right MTInt
    ),
    ( MyApp
        ( MyLambda
            (mkName "x")
            ( (MyIf (MyVar (mkName "x")) (int 10) (int 10))
            )
        )
        (int 100),
      Left "Can't match MTBool with MTInt"
    ),
    ( MyLambda (mkName "x") (MyApp (MyVar (mkName "x")) (MyVar (mkName "x"))),
      Left "U1 fails occurs check"
    ),
    (MyPair (int 1) (bool True), Right (MTPair MTInt MTBool)),
    ( MyLetPair (mkName "a") (mkName "b") (MyPair (int 1) (bool True)) (MyVar (mkName "a")),
      Right MTInt
    ),
    ( MyLambda
        (mkName "x")
        ( MyLetPair
            (mkName "a")
            (mkName "b")
            (MyVar (mkName "x"))
            (MyVar (mkName "a"))
        ),
      Right (MTFunction (MTPair (unknown 2) (unknown 3)) (unknown 2))
    ),
    ( MyLet
        (mkName "fst")
        (MyLambda (mkName "tuple") (MyLetPair (mkName "a") (mkName "b") (MyVar (mkName "tuple")) (MyVar (mkName "a"))))
        ( MyLet
            (mkName "x")
            (MyPair (int 1) (int 2))
            (MyApp (MyVar (mkName "fst")) (MyVar (mkName "x")))
        ),
      Right MTInt
    )
  ]

identity :: Expr
identity = (MyLambda (mkName "x") (MyVar (mkName "x")))

spec :: Spec
spec = do
  describe "Typechecker" $ do
    it "Our expressions typecheck as expected" $ do
      _ <- traverse (\(code, expected) -> startInference code `shouldBe` expected) exprs
      pure ()
    it "We can use identity with two different datatypes in one expression" $ do
      let lambda =
            ( MyLambda
                (mkName "x")
                ( MyIf
                    (MyApp identity (MyVar (mkName "x")))
                    (MyApp identity (int 1))
                    (MyApp identity (int 2))
                )
            )
      let expr = MyApp lambda (bool True)
      (startInference lambda) `shouldBe` Right (MTFunction MTBool MTInt)
      (startInference expr) `shouldBe` Right MTInt
{-  describe "Serialisation" $ do
it "Round trip" $ do
  property $ \x -> JSON.decode (JSON.encode x) == (Just x :: Maybe Expr)
-}
