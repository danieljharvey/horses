{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Typechecker
  ( spec,
  )
where

import Data.Foldable (traverse_)
import qualified Data.Map as M
import Language.Mimsa
import Language.Mimsa.Types
import Test.Helpers
import Test.Hspec
import Test.QuickCheck.Instances ()

exprs :: (Monoid ann) => [(Expr Variable ann, Either TypeError MonoType)]
exprs =
  [ (int 1, Right (MTPrim MTInt)),
    (bool True, Right (MTPrim MTBool)),
    ( str
        (StringType "hello"),
      Right (MTPrim MTString)
    ),
    -- (MyVar (named "x"), Left "Unknown variable \"x\""),
    (MyLet mempty (named "x") (int 42) (bool True), Right (MTPrim MTBool)),
    (MyLet mempty (named "x") (int 42) (MyVar mempty (named "x")), Right (MTPrim MTInt)),
    ( MyLet
        mempty
        (named "x")
        (bool True)
        (MyLet mempty (named "y") (int 42) (MyVar mempty (named "x"))),
      Right (MTPrim MTBool)
    ),
    ( MyLet
        mempty
        (named "x")
        (bool True)
        (MyLet mempty (named "x") (int 42) (MyVar mempty (named "x"))),
      Right (MTPrim MTInt)
    ),
    ( MyLambda mempty (named "x") (bool True),
      Right $ MTFunction (unknown 1) (MTPrim MTBool)
    ),
    ( identity,
      Right $ MTFunction (unknown 1) (unknown 1)
    ),
    ( MyLambda mempty (named "x") (MyLambda mempty (named "y") (MyVar mempty (named "x"))),
      Right $
        MTFunction
          (unknown 1)
          (MTFunction (unknown 2) (unknown 1))
    ),
    ( MyApp
        mempty
        ( MyLambda
            mempty
            (named "x")
            (bool True)
        )
        (int 1),
      Right (MTPrim MTBool)
    ),
    ( MyApp
        mempty
        identity
        (int 1),
      Right (MTPrim MTInt)
    ),
    ( MyApp
        mempty
        ( MyLambda
            mempty
            (named "x")
            (MyIf mempty (MyVar mempty (named "x")) (int 10) (int 10))
        )
        (int 100),
      Left $ UnificationError (MTPrim MTBool) (MTPrim MTInt)
    ),
    ( MyLambda mempty (named "x") (MyApp mempty (MyVar mempty (named "x")) (MyVar mempty (named "x"))),
      Left $
        FailsOccursCheck
          mempty
          (tvFree 1)
          ( MTFunction
              (MTVar (tvFree 1))
              (MTVar (tvFree 2))
          )
    ),
    (MyPair mempty (int 1) (bool True), Right (MTPair (MTPrim MTInt) (MTPrim MTBool))),
    ( MyLetPair mempty (named "a") (named "b") (MyPair mempty (int 1) (bool True)) (MyVar mempty (named "a")),
      Right (MTPrim MTInt)
    ),
    ( MyLambda
        mempty
        (named "x")
        ( MyLetPair
            mempty
            (named "a")
            (named "b")
            (MyVar mempty (named "x"))
            (MyVar mempty (named "a"))
        ),
      Right (MTFunction (MTPair (unknown 2) (unknown 3)) (unknown 2))
    ),
    ( MyLet
        mempty
        (named "fst")
        ( MyLambda
            mempty
            (named "tuple")
            ( MyLetPair
                mempty
                (named "a")
                (named "b")
                (MyVar mempty (named "tuple"))
                (MyVar mempty (named "a"))
            )
        )
        ( MyLet
            mempty
            (named "x")
            (MyPair mempty (int 1) (int 2))
            (MyApp mempty (MyVar mempty (named "fst")) (MyVar mempty (named "x")))
        ),
      Right (MTPrim MTInt)
    ),
    ( MyRecord
        mempty
        mempty,
      Right $
        MTRecord mempty
    ),
    ( MyRecord
        mempty
        ( M.fromList
            [ (mkName "dog", int 1),
              (mkName "cat", int 2)
            ]
        ),
      Right $
        MTRecord
          ( M.fromList
              [ (mkName "dog", (MTPrim MTInt)),
                (mkName "cat", (MTPrim MTInt))
              ]
          )
    ),
    ( MyLambda
        mempty
        (named "i")
        ( MyIf
            mempty
            ( MyRecordAccess
                mempty
                (MyVar mempty (named "i"))
                (mkName "dog")
            )
            (int 1)
            (int 2)
        ),
      Right $ MTFunction (MTRecord $ M.singleton (mkName "dog") (MTPrim MTBool)) (MTPrim MTInt)
    ),
    ( MyLambda
        mempty
        (named "i")
        ( MyIf
            mempty
            ( MyRecordAccess
                mempty
                (MyVar mempty (named "i"))
                (mkName "dog")
            )
            ( MyIf
                mempty
                (MyRecordAccess mempty (MyVar mempty (named "i")) (mkName "cat"))
                (int 1)
                (int 2)
            )
            (int 3)
        ),
      Left $
        MissingRecordTypeMember
          (mkName "cat")
          ( M.singleton
              (mkName "dog")
              (unknown 2)
          )
    )
    -- combining multiple facts about an unknown record is for later
  ]

identity :: Monoid ann => Expr Variable ann
identity = MyLambda mempty (named "x") (MyVar mempty (named "x"))

spec :: Spec
spec =
  describe "Typechecker" $ do
    it "Our expressions typecheck as expected" $
      traverse_
        ( \(code, expected) ->
            --T.putStrLn (prettyPrint code)
            startInference mempty code `shouldBe` expected
        )
        exprs
    it "Uses a polymorphic function twice with conflicting types" $ do
      let expr =
            MyLet
              mempty
              (named "id")
              (MyLambda mempty (named "a") (MyVar mempty (named "a")))
              ( MyPair
                  mempty
                  (MyApp mempty (MyVar mempty (named "id")) (int 1))
                  (MyApp mempty (MyVar mempty (named "id")) (bool True))
              )
      let expected = Right (MTPair (MTPrim MTInt) (MTPrim MTBool))
      startInference mempty expr `shouldBe` expected
    it "We can use identity with two different datatypes in one expression" $ do
      let lambda =
            MyLambda
              mempty
              (named "x")
              ( MyIf
                  mempty
                  (MyApp mempty identity (MyVar mempty (named "x")))
                  (MyApp mempty identity (int 1))
                  (MyApp mempty identity (int 2))
              )
      let expr = MyApp mempty lambda (bool True)
      startInference mempty lambda `shouldBe` Right (MTFunction (MTPrim MTBool) (MTPrim MTInt))
      startInference mempty expr `shouldBe` Right (MTPrim MTInt)
{-  describe "Serialisation" $ do
it "Round trip" $ do
  property $ \x -> JSON.decode (JSON.encode x) == (Just x :: Maybe Expr)
-}
