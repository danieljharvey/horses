{-# LANGUAGE OverloadedStrings #-}

module Test.ParserSpec (spec) where

import Data.Foldable (traverse_)
import Data.Functor
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Smol.Core
import Test.Helpers
import Test.Hspec

spec :: Spec
spec = do
  describe "Parser" $ do
    describe "Constraint" $ do
      let inputs =
            [ ( "Eq Int",
                Constraint "Eq" [tyInt]
              ),
              ( "Eq (a,b)",
                Constraint "Eq" [tyTuple (tyVar "a") [tyVar "b"]]
              )
            ]
      traverse_
        ( \(input, expected) ->
            it (T.unpack $ "Parses constraint: " <> input) $ do
              parseConstraintAndFormatError input `shouldBe` Right expected
        )
        inputs

    describe "Expr" $ do
      let strings =
            [ ("True", bool True),
              ("False", bool False),
              ("Unit", unit),
              ("-1", int (-1)),
              ("100", int 100),
              ("if True then 1 else 2", EIf () (bool True) (int 1) (int 2)),
              ("1 + 2", EInfix () OpAdd (int 1) (int 2)),
              ("1 + 2 + 3", EInfix () OpAdd (EInfix () OpAdd (int 1) (int 2)) (int 3)),
              ("\"\"", EPrim () (PString mempty)),
              ("\"horses\"", EPrim () (PString "horses")),
              ("(True)", bool True),
              ("(True,True)", tuple (bool True) [bool True]),
              ("(100, 200, 300)", tuple (int 100) [int 200, int 300]),
              ("log", var "log"),
              ("Prelude.log", EVar () (ParseDep "log" (Just "Prelude"))),
              ("\\a -> True", ELambda () "a" (bool True)),
              ( "(\\a -> True : Int -> Bool)",
                EAnn () (TFunc () mempty tyInt tyBool) (ELambda () "a" (bool True))
              ),
              ( "(\\a -> a : a -> a)",
                EAnn () (TFunc () mempty (tyVar "a") (tyVar "a")) (ELambda () "a" (var "a"))
              ),
              ("{ a: 1, b: True }", ERecord () (M.fromList [("a", int 1), ("b", bool True)])),
              ("Just", constructor "Just"),
              ("Maybe.Just", EConstructor () (ParseDep "Just" (Just "Maybe"))),
              ("Just True", EApp () (constructor "Just") (bool True)),
              ("These 1 False", EApp () (EApp () (constructor "These") (int 1)) (bool False)),
              ( "case a {(b, c) -> b + c}",
                patternMatch (var "a") [(PTuple () (PVar () "b") (NE.fromList [PVar () "c"]), EInfix () OpAdd (var "b") (var "c"))]
              ),
              ( "let (a, _) = (1,2); a",
                patternMatch (tuple (int 1) [int 2]) [(PTuple () (PVar () "a") (NE.fromList [PWildcard ()]), var "a")]
              ),
              ( "case (1,2) {(a,_) -> a }",
                patternMatch (tuple (int 1) [int 2]) [(PTuple () (PVar () "a") (NE.fromList [PWildcard ()]), var "a")]
              ),
              ( "case (1,2) {(a,_) -> { let b = 1 in a } }",
                patternMatch (tuple (int 1) [int 2]) [(PTuple () (PVar () "a") (NE.fromList [PWildcard ()]), ELet () "b" (int 1) (var "a"))]
              ),
              ( "case (True, 1) {(True, a) -> a, (False,_) -> 0}",
                patternMatch
                  (tuple (bool True) [int 1])
                  [ (PTuple () (PLiteral () (PBool True)) (NE.fromList [PVar () "a"]), var "a"),
                    (PTuple () (PLiteral () (PBool False)) (NE.fromList [PWildcard ()]), int 0)
                  ]
              ),
              ( "case [1,2,3] { [_, ...b] -> b, other -> other }",
                patternMatch
                  (array [int 1, int 2, int 3])
                  [ (PArray () [PWildcard ()] (SpreadValue () "b"), var "b"),
                    (PVar () "other", var "other")
                  ]
              ),
              ("let a = 1 in a", ELet () "a" (int 1) (var "a")),
              ("let a = { let b = 1 in 1 } in a", ELet () "a" (ELet () "b" (int 1) (int 1)) (var "a")),
              ( "let id a = a in True",
                ELet () "id" (ELambda () "a" (var "a")) (bool True)
              ),
              ("f (a b)", EApp () (var "f") (EApp () (var "a") (var "b"))),
              ("fmap inc (Just 1)", EApp () (EApp () (var "fmap") (var "inc")) (EApp () (constructor "Just") (int 1))),
              ("Just (1 + 1)", EApp () (constructor "Just") (EInfix () OpAdd (int 1) (int 1))),
              ("[]", EArray () mempty),
              ("[1,2,3,4]", EArray () (Seq.fromList [int 1, int 2, int 3, int 4])),
              ( "\\(a,_) -> a",
                ELambda
                  ()
                  "lambdaArg"
                  ( EPatternMatch
                      ()
                      (var "lambdaArg")
                      ( NE.singleton
                          ( PTuple () (PVar () "a") (NE.singleton $ PWildcard ()),
                            var "a"
                          )
                      )
                  )
              )
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
              ("1 | 2 | 3", tyIntLit [1, 2, 3]),
              ("\"horse\"", tyStrLit ["horse"]),
              ("1 + 2", TInfix () OpAdd (tyIntLit [1]) (tyIntLit [2])),
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
              ),
              ( "(a -> b) -> f a -> f b",
                TFunc
                  ()
                  mempty
                  (TFunc () mempty (TVar () "a") (TVar () "b"))
                  ( TFunc
                      ()
                      mempty
                      (TApp () (TVar () "f") (TVar () "a"))
                      (TApp () (TVar () "f") (TVar () "b"))
                  )
              ),
              ("Maybe.Maybe", TConstructor () (ParseDep "Maybe" (Just "Maybe"))),
              ("[Bool]", TArray () 0 tyBool),
              ("String", TPrim () TPString),
              ("Either e a", tyCons "Either" [tyVar "e", tyVar "a"]),
              ( "s -> (a, s)",
                TFunc () mempty (tyVar "s") (tyTuple (tyVar "a") [tyVar "s"])
              ),
              ( "(b -> c) -> (a -> b)",
                TFunc () mempty (TFunc () mempty (tyVar "b") (tyVar "c")) (TFunc () mempty (tyVar "a") (tyVar "b"))
              ),
              ( "a -> State s a",
                TFunc () mempty (tyVar "a") (tyCons "State" [tyVar "s", tyVar "a"])
              )
            ]
      traverse_
        ( \(str, ty) -> it (T.unpack str) $ do
            case parseTypeAndFormatError str of
              Right parsedTy -> parsedTy $> () `shouldBe` ty
              Left e -> error (T.unpack e)
        )
        strings

    describe "DataType" $ do
      let strings =
            [ ( "type Expr ann = EInt ann Int",
                DataType "Expr" ["ann"] (M.singleton "EInt" [TVar () "ann", TPrim () TPInt])
              )
            ]
      traverse_
        ( \(str, dt) -> it (T.unpack str) $ do
            case parseDataTypeAndFormatError str of
              Right parsedDt -> parsedDt $> () `shouldBe` dt
              Left e -> error (T.unpack e)
        )
        strings
