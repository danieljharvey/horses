{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.ParserSpec (spec) where

import Data.Bifunctor (second)
import Data.Either (isRight)
import Data.FileEmbed
import Data.Foldable (traverse_)
import Data.Functor
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Smol.Core
import Test.Helpers
import Test.Hspec

-- these are saved in a file that is included in compilation
testInputs :: [(FilePath, Text)]
testInputs =
  fmap (second T.decodeUtf8) $(makeRelativeToProject "test/static/" >>= embedDir)

spec :: Spec
spec = do
  describe "Parser" $ do
    describe "Module" $ do
      let singleDefs =
            [ "type Dog a = Woof String | Other a",
              "def id : a -> a",
              "def id a = a",
              "def compose f g a = f (g a)",
              "def compose : (c -> b) -> (a -> b) -> (a -> c)"
            ]

      it "All defs" $ do
        let result = parseModuleAndFormatError (T.intercalate "\n" (T.pack <$> singleDefs))
        result `shouldSatisfy` isRight

      traverse_
        ( \input -> it ("Parses module item: " <> input) $ do
            let result = parseModuleAndFormatError (T.pack input)

            result `shouldSatisfy` isRight
        )
        singleDefs

      traverse_
        ( \(filename, contents) ->
            it ("Parses " <> filename) $ do
              let result = parseModuleAndFormatError contents
              result `shouldSatisfy` isRight
        )
        testInputs

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
              ("\"\"", EPrim () (PString mempty)),
              ("\"horses\"", EPrim () (PString "horses")),
              ("(True)", bool True),
              ("(True,True)", tuple (bool True) [bool True]),
              ("(100, 200, 300)", tuple (nat 100) [nat 200, nat 300]),
              ("log", var "log"),
              ("Prelude.log", EVar () (ParseDep "log" (Just "Prelude"))),
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
              ("Maybe.Just", EConstructor () (ParseDep "Just" (Just "Maybe"))),
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
              ( "case [1,2,3] of [_, ...b] -> b | other -> other",
                patternMatch
                  (array [nat 1, nat 2, nat 3])
                  [ (PArray () [PWildcard ()] (SpreadValue () "b"), var "b"),
                    (PVar () "other", var "other")
                  ]
              ),
              ("let a = 1 in a", ELet () "a" (nat 1) (var "a")),
              ("f (a b)", EApp () (var "f") (EApp () (var "a") (var "b"))),
              ("fmap inc (Just 1)", EApp () (EApp () (var "fmap") (var "inc")) (EApp () (constructor "Just") (nat 1))),
              ("Just (1 + 1)", EApp () (constructor "Just") (EInfix () OpAdd (nat 1) (nat 1))),
              ("[]", EArray () mempty),
              ("[1,2,3,4]", EArray () (Seq.fromList [nat 1, nat 2, nat 3, nat 4]))
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
              ("{ egg: 42 } => 42", TGlobals () (M.singleton "egg" (tyIntLit [42])) (tyIntLit [42])),
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
              ("Maybe.Maybe", TConstructor () (ParseDep "Maybe" (Just "Maybe"))),
              ("[Bool]", TArray () 0 tyBool),
              ("String", TPrim () TPString),
              ("Either e a", tyCons "Either" [tyVar "e", tyVar "a"]),
              ("s -> (a, s)", TFunc () mempty (tyVar "s") (tyTuple (tyVar "a") [tyVar "s"])),
              ("(b -> c) -> (a -> b)", TFunc () mempty (TFunc () mempty (tyVar "b") (tyVar "c")) (TFunc () mempty (tyVar "a") (tyVar "b")))
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
