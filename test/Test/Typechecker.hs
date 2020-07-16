{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Typechecker
  ( spec,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State (runState)
-- import qualified Data.Aeson as JSON
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
--import qualified Data.Text.IO as T
import Language.Mimsa
import Language.Mimsa.Typechecker.Infer
import Language.Mimsa.Types
import Test.Helpers
import Test.Hspec
import Test.QuickCheck.Instances ()

exprs :: [(Expr Variable, Either TypeError MonoType)]
exprs =
  [ (int 1, Right MTInt),
    (bool True, Right MTBool),
    ( str
        (StringType "hello"),
      Right MTString
    ),
    -- (MyVar (named "x"), Left "Unknown variable \"x\""),
    (MyLet (named "x") (int 42) (bool True), Right MTBool),
    (MyLet (named "x") (int 42) (MyVar (named "x")), Right MTInt),
    ( MyLet
        (named "x")
        (bool True)
        (MyLet (named "y") (int 42) (MyVar (named "x"))),
      Right MTBool
    ),
    ( MyLet
        (named "x")
        (bool True)
        (MyLet (named "x") (int 42) (MyVar (named "x"))),
      Right MTInt
    ),
    ( MyLambda (named "x") (bool True),
      Right $ MTFunction (unknown 1) MTBool
    ),
    ( identity,
      Right $ MTFunction (unknown 1) (unknown 1)
    ),
    ( MyLambda (named "x") (MyLambda (named "y") (MyVar (named "x"))),
      Right $
        MTFunction
          (unknown 1)
          (MTFunction (unknown 2) (unknown 1))
    ),
    ( MyApp
        ( MyLambda
            (named "x")
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
            (named "x")
            ( (MyIf (MyVar (named "x")) (int 10) (int 10))
            )
        )
        (int 100),
      Left $ UnificationError MTBool MTInt
    ),
    ( MyLambda (named "x") (MyApp (MyVar (named "x")) (MyVar (named "x"))),
      Left $ FailsOccursCheck mempty (TVNumber 1) (MTFunction (MTVar (TVNumber 1)) (MTVar (TVNumber 2)))
    ),
    (MyPair (int 1) (bool True), Right (MTPair MTInt MTBool)),
    ( MyLetPair (named "a") (named "b") (MyPair (int 1) (bool True)) (MyVar (named "a")),
      Right MTInt
    ),
    ( MyLambda
        (named "x")
        ( MyLetPair
            (named "a")
            (named "b")
            (MyVar (named "x"))
            (MyVar (named "a"))
        ),
      Right (MTFunction (MTPair (unknown 2) (unknown 3)) (unknown 2))
    ),
    ( MyLet
        (named "fst")
        (MyLambda (named "tuple") (MyLetPair (named "a") (named "b") (MyVar (named "tuple")) (MyVar (named "a"))))
        ( MyLet
            (named "x")
            (MyPair (int 1) (int 2))
            (MyApp (MyVar (named "fst")) (MyVar (named "x")))
        ),
      Right MTInt
    ),
    ( MyCase
        (MySum MyLeft (int 1))
        ( MyLambda
            (named "l")
            (MySum MyLeft (MyVar (named "l")))
        )
        ( MyLambda (named "r") (MySum MyRight (MyVar (named "r")))
        ),
      Right $ MTSum MTInt (unknown 1)
    ),
    ( MyCase
        (MySum MyLeft (int 1))
        (MyLambda (named "l") (str' "Left!"))
        (MyLambda (named "r") (str' "Right!")),
      Right MTString
    ),
    ( MyCase
        (MySum MyRight (int 1))
        (MyLambda (named "l") (str' "Left!"))
        (MyLambda (named "r") (str' "Right!")),
      Right MTString
    ),
    ( MyLetList
        (named "head")
        (named "tail")
        (MyList $ NE.fromList [int 1])
        (MyVar (named "head")),
      Right $ MTInt
    ),
    ( MyLetList
        (named "head")
        (named "tail")
        (MyList $ NE.fromList [int 1])
        (MyVar (named "tail")),
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
        (named "i")
        ( MyIf
            ( MyRecordAccess
                (MyVar (named "i"))
                (mkName "dog")
            )
            (int 1)
            (int 2)
        ),
      Right $ MTFunction (MTRecord $ M.singleton (mkName "dog") MTBool) MTInt
    ),
    ( MyLambda
        (named "i")
        ( MyIf
            ( MyRecordAccess
                (MyVar (named "i"))
                (mkName "dog")
            )
            ( MyIf
                (MyRecordAccess (MyVar (named "i")) (mkName "cat"))
                (int 1)
                (int 2)
            )
            (int 3)
        ),
      Left $
        MissingRecordTypeMember
          ( (mkName "cat")
          )
          ( M.singleton
              (mkName "dog")
              (unknown 2)
          )
    )
    -- combining multiple facts about an unknown record is for later
  ]

identity :: Expr Variable
identity = (MyLambda (named "x") (MyVar (named "x")))

runInstantiate :: Scheme -> Either TypeError (Substitutions, MonoType)
runInstantiate scheme =
  fst $ runState (runReaderT (runExceptT (instantiate scheme)) mempty) 2

spec :: Spec
spec = do
  describe "Typechecker" $ do
    it "Our expressions typecheck as expected" $ do
      _ <-
        traverse
          ( \(code, expected) -> do
              --T.putStrLn (prettyPrint code)
              startInference mempty code `shouldBe` expected
          )
          exprs
      pure ()
    it "Instantiate with no problems" $ do
      let scheme = Scheme [] (MTVar (TVNumber 1))
      snd <$> runInstantiate scheme `shouldBe` Right (MTVar (TVNumber 1))
    it "Instantiate with no matches" $ do
      let scheme = Scheme [TVNumber 2] (MTVar (TVNumber 1))
      snd <$> runInstantiate scheme `shouldBe` Right (MTVar (TVNumber 1))
    it "Instantiate" $ do
      let scheme = Scheme [TVNumber 1] (MTVar (TVNumber 1))
      snd <$> runInstantiate scheme `shouldBe` Right (MTVar (TVNumber 2))
    {-it "Uses a polymorphic function twice with conflicting types" $ do
          let expr =
                MyLet
                  (named "id")
                  (MyLambda (named "a") (MyVar (named "a")))
                  ( MyPair
                      (MyApp (MyVar (named "id")) (int 1))
                      (MyApp (MyVar (named "id")) (bool True))
                  )
          let expected = Right (MTPair MTInt MTBool)
          startInference mempty expr `shouldBe` expected
    -}
    it "We can use identity with two different datatypes in one expression" $ do
      let lambda =
            ( MyLambda
                (named "x")
                ( MyIf
                    (MyApp identity (MyVar (named "x")))
                    (MyApp identity (int 1))
                    (MyApp identity (int 2))
                )
            )
      let expr = MyApp lambda (bool True)
      (startInference mempty lambda) `shouldBe` Right (MTFunction MTBool MTInt)
      (startInference mempty expr) `shouldBe` Right MTInt
{-  describe "Serialisation" $ do
it "Round trip" $ do
  property $ \x -> JSON.decode (JSON.encode x) == (Just x :: Maybe Expr)
-}
