{-# LANGUAGE OverloadedStrings #-}

module Test.ParserSpec (spec) where

import Data.Foldable (traverse_)
import Data.Functor
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Smol.Core
import Test.Helpers
import Test.Hspec

spec :: Spec
spec = do
  describe "Parser" $ do
    describe "Expr" $ do
      let strings =
            [ ("True", bool True),
              ("False", bool False),
              ("Unit", unit),
              ("-1", int (-1)),
              ("100", nat 100),
              ("if True then 1 else 2", EIf () (bool True) (nat 1) (nat 2)),
              ("1 + 2", EInfix () OpAdd (nat 1) (nat 2)),
              ("1 + 2 + 3", EInfix () OpAdd (EInfix () OpAdd (nat 1) (nat 2)) (nat 3)),
              ("(True)", bool True),
              ("(True,True)", tuple (bool True) [bool True]),
              ("(100, 200, 300)", tuple (nat 100) [nat 200, nat 300]),
              ("log", var "log"),
              ("\\a -> True", ELambda () "a" (bool True)),
              ( "(\\a -> True : Int -> Bool)",
                EAnn () (TFunc () mempty tyInt tyBool) (ELambda () "a" (bool True))
              ),
              ( "(\\a -> a : a -> a)",
                EAnn () (TFunc () mempty (tyVar "a") (tyVar "a")) (ELambda () "a" (var "a"))
              ),
              ("dog!", EGlobal () "dog"),
              ("{ a: 1, b: True }", ERecord () (M.fromList [("a", nat 1), ("b", bool True)])),
              ("Just", constructor "Just"),
              ("Just True", EApp () (constructor "Just") (bool True)),
              ("These 1 False", EApp () (EApp () (constructor "These") (nat 1)) (bool False)),
              ( "case a of (b, c) -> b + c",
                patternMatch (var "a") [(PTuple () (PVar () "b") (NE.fromList [PVar () "c"]), EInfix () OpAdd (var "b") (var "c"))]
              ),
              ( "case (1,2) of (a,_) -> a",
                patternMatch (tuple (nat 1) [nat 2]) [(PTuple () (PVar () "a") (NE.fromList [PWildcard ()]), var "a")]
              ),
              ( "case (True, 1) of (True, a) -> a | (False,_) -> 0",
                patternMatch
                  (tuple (bool True) [nat 1])
                  [ (PTuple () (PLiteral () (PBool True)) (NE.fromList [PVar () "a"]), var "a"),
                    (PTuple () (PLiteral () (PBool False)) (NE.fromList [PWildcard ()]), nat 0)
                  ]
              ),
              ("let a = 1 in a", ELet () "a" (nat 1) (var "a")),
              ("f (a b)", EApp () (var "f") (EApp () (var "a") (var "b"))),
              ("fmap inc (Just 1)", EApp () (EApp () (var "fmap") (var "inc")) (EApp () (constructor "Just") (nat 1))),
              ("Just (1 + 1)", EApp () (constructor "Just") (EInfix () OpAdd (nat 1) (nat 1)))
            ]
      traverse_
        ( \(str, expr) -> it (T.unpack str) $ do
            case parseExprAndFormatError str of
              Right parsedExp -> parsedExp $> () `shouldBe` expr
              Left e -> error (T.unpack e)
        )
        strings

    describe "Type" $ do
      let strings =
            [ ("True", tyBoolLit True),
              ("False", tyBoolLit False),
              ( "(a -> b) -> Maybe a -> Maybe b",
                TFunc
                  ()
                  mempty
                  ( TFunc () mempty (tyVar "a") (tyVar "b")
                  )
                  (TFunc () mempty (tyCons "Maybe" [tyVar "a"]) (tyCons "Maybe" [tyVar "b"]))
              ),
              ( "m (a -> b)",
                TApp
                  ()
                  (tyVar "m")
                  ( TFunc () mempty (TVar () "a") (TVar () "b")
                  )
              ),
              ( "m (a -> b) -> m a -> m b",
                TFunc
                  ()
                  mempty
                  (TApp () (tyVar "m") (TFunc () mempty (TVar () "a") (TVar () "b")))
                  ( TFunc
                      ()
                      mempty
                      (TApp () (TVar () "m") (TVar () "a"))
                      (TApp () (TVar () "m") (TVar () "b"))
                  )
              )
            ]
      traverse_
        ( \(str, ty) -> it (T.unpack str) $ do
            case parseTypeAndFormatError str of
              Right parsedTy -> parsedTy $> () `shouldBe` ty
              Left e -> error (T.unpack e)
        )
        strings
