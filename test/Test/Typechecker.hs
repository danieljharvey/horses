{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Typechecker
  ( spec,
  )
where

-- import qualified Data.Aeson as JSON
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
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
      Left "Unification error: Can't match MTBool with MTInt"
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
    ),
    ( MyCase
        (MySum MyLeft (int 1))
        ( MyLambda
            (mkName "l")
            (MySum MyLeft (MyVar (mkName "l")))
        )
        ( MyLambda (mkName "r") (MySum MyRight (MyVar (mkName "r")))
        ),
      Right $ MTSum MTInt (MTVar (mkName "U1"))
    ),
    ( MyCase
        (MySum MyLeft (int 1))
        (MyLambda (mkName "l") (str' "Left!"))
        (MyLambda (mkName "r") (str' "Right!")),
      Right MTString
    ),
    ( MyCase
        (MySum MyRight (int 1))
        (MyLambda (mkName "l") (str' "Left!"))
        (MyLambda (mkName "r") (str' "Right!")),
      Right MTString
    ),
    ( MyLetList
        (mkName "head")
        (mkName "tail")
        (MyList $ NE.fromList [int 1])
        (MyVar (mkName "head")),
      Right $ MTInt
    ),
    ( MyLetList
        (mkName "head")
        (mkName "tail")
        (MyList $ NE.fromList [int 1])
        (MyVar (mkName "tail")),
      Right $ (MTSum MTUnit (MTList MTInt))
    ),
    ( MyRecord
        mempty,
      Right $
        MTRecord mempty
    ),
    ( MyRecord
        ( M.fromList
            [ (mkName "dog", int 1),
              (mkName "cat", int 2)
            ]
        ),
      Right $
        MTRecord
          ( M.fromList
              [ (mkName "dog", MTInt),
                (mkName "cat", MTInt)
              ]
          )
    ),
    ( MyLambda
        (mkName "i")
        ( MyIf
            ( MyRecordAccess
                (MyVar (mkName "i"))
                (mkName "dog")
            )
            (int 1)
            (int 2)
        ),
      Right $ MTFunction (MTRecord $ M.singleton (mkName "dog") MTBool) MTInt
    ),
    ( MyLambda
        (mkName "i")
        ( MyIf
            ( MyRecordAccess
                (MyVar (mkName "i"))
                (mkName "dog")
            )
            ( MyIf
                (MyRecordAccess (MyVar (mkName "i")) (mkName "cat"))
                (int 1)
                (int 2)
            )
            (int 3)
        ),
      Right $
        MTFunction
          ( MTRecord $
              M.fromList
                [ (mkName "dog", MTBool),
                  (mkName "cat", MTBool)
                ]
          )
          MTInt
    )
  ]

identity :: Expr
identity = (MyLambda (mkName "x") (MyVar (mkName "x")))

spec :: Spec
spec = do
  describe "Typechecker" $ do
    it "Our expressions typecheck as expected" $ do
      _ <-
        traverse
          ( \(code, expected) -> do
              -- T.putStrLn (prettyPrint code)
              startInference code `shouldBe` expected
          )
          exprs
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
