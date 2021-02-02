{-# LANGUAGE OverloadedStrings #-}

module Test.Typechecker.Codegen
  ( spec,
  )
where

import Data.Bifunctor (first)
import Data.Either (isRight)
import Data.Functor
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Text (Text)
import Language.Mimsa.Actions
import Language.Mimsa.Printer
import Language.Mimsa.Typechecker.Codegen
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.ResolvedExpression
import Test.Data.Project
import Test.Hspec
import Test.Utils.Helpers

-- | has no constructors, we can do nothing with this
dtVoid :: DataType
dtVoid = DataType "Void" mempty mempty

-- | an enum, we can go to and from a string
dtTrafficLights :: DataType
dtTrafficLights =
  DataType
    "TrafficLights"
    mempty
    ( M.fromList
        [ ("Red", mempty),
          ("Yellow", mempty),
          ("Green", mempty)
        ]
    )

-- | A newtype around a string
-- | we can wrap and unwrap maybe?
dtWrappedString :: DataType
dtWrappedString =
  DataType
    "WrappedString"
    mempty
    (M.singleton "Wrapped" [VarName "String"])

-- | Identity monad
dtIdentity :: DataType
dtIdentity =
  DataType
    "Identity"
    ["a"]
    (M.singleton "Identity" [VarName "a"])

-- | Maybe monad
dtMaybe :: DataType
dtMaybe =
  DataType
    "Maybe"
    ["a"]
    (M.fromList [("Just", [VarName "a"]), ("Nothing", [])])

-- | These monad
dtThese :: DataType
dtThese =
  DataType
    "These"
    ["a", "b"]
    ( M.fromList
        [ ("This", [VarName "a"]),
          ("That", [VarName "b"]),
          ("These", [VarName "a", VarName "b"])
        ]
    )

typecheckInstance ::
  (DataType -> Either Text (Expr Name ())) ->
  DataType ->
  Either (Error Annotation) (ResolvedExpression Annotation)
typecheckInstance mkInstance dt =
  inst'
    >>= (\expr -> getTypecheckedStoreExpression (prettyPrint expr) stdLib expr)
  where
    inst' =
      first ParseError (fmap ($> mempty) (mkInstance dt))

spec :: Spec
spec = do
  describe "Codegen" $ do
    describe "Enum instances" $ do
      it "Generates toString for dtTrafficLights" $ do
        typecheckInstance toString dtTrafficLights `shouldSatisfy` isRight
        toString dtTrafficLights
          `shouldBe` Right
            ( MyLambda
                mempty
                "trafficLights"
                ( MyCaseMatch
                    mempty
                    (MyVar mempty "trafficLights")
                    ( NE.fromList
                        [ ("Green", str "Green"),
                          ("Red", str "Red"),
                          ("Yellow", str "Yellow")
                        ]
                    )
                    Nothing
                )
            )
    describe "Newtype instances" $ do
      it "Generates wrap for dtWrappedString" $ do
        typecheckInstance wrap dtWrappedString `shouldSatisfy` isRight
        wrap dtWrappedString
          `shouldBe` Right
            ( MyLambda
                mempty
                "a"
                ( MyConsApp
                    mempty
                    (MyConstructor mempty "Wrapped")
                    (MyVar mempty "a")
                )
            )
      it "Generates unwrap for dtWrappedString" $ do
        typecheckInstance unwrap dtWrappedString `shouldSatisfy` isRight
        unwrap dtWrappedString
          `shouldBe` Right
            ( MyLambda
                mempty
                "wrappedString"
                ( MyCaseMatch
                    mempty
                    (MyVar mempty "wrappedString")
                    ( pure ("Wrapped", MyLambda mempty "a" (MyVar mempty "a"))
                    )
                    Nothing
                )
            )
    describe "Functor instances" $ do
      it "Generates functorMap for dtIdentity" $ do
        typecheckInstance functorMap dtIdentity `shouldSatisfy` isRight
        functorMap dtIdentity
          `shouldBe` Right
            ( MyLambda
                mempty
                "f"
                ( MyLambda
                    mempty
                    "identity"
                    ( MyCaseMatch
                        mempty
                        (MyVar mempty "identity")
                        ( pure
                            ( "Identity",
                              MyLambda
                                mempty
                                "a"
                                ( MyConsApp
                                    mempty
                                    (MyConstructor mempty "Identity")
                                    ( MyApp
                                        mempty
                                        (MyVar mempty "f")
                                        (MyVar mempty "a")
                                    )
                                )
                            )
                        )
                        Nothing
                    )
                )
            )
      it "Generates functorMap for dtMaybe" $ do
        typecheckInstance functorMap dtMaybe `shouldSatisfy` isRight
        functorMap dtMaybe
          `shouldBe` Right
            ( MyLambda
                mempty
                "f"
                ( MyLambda
                    mempty
                    "maybe"
                    ( MyCaseMatch
                        mempty
                        (MyVar mempty "maybe")
                        ( NE.fromList
                            [ ( "Just",
                                MyLambda
                                  mempty
                                  "a"
                                  ( MyConsApp
                                      mempty
                                      (MyConstructor mempty "Just")
                                      ( MyApp
                                          mempty
                                          (MyVar mempty "f")
                                          (MyVar mempty "a")
                                      )
                                  )
                              ),
                              ("Nothing", MyConstructor mempty "Nothing")
                            ]
                        )
                        Nothing
                    )
                )
            )
      it "Generates functorMap for dtThese" $ do
        typecheckInstance functorMap dtThese `shouldSatisfy` isRight
        functorMap dtThese
          `shouldBe` Right
            ( MyLambda
                mempty
                "f"
                ( MyLambda
                    mempty
                    "these"
                    ( MyCaseMatch
                        mempty
                        (MyVar mempty "these")
                        ( NE.fromList
                            [ ( "That",
                                MyLambda
                                  mempty
                                  "b"
                                  ( MyConsApp
                                      mempty
                                      (MyConstructor mempty "That")
                                      ( MyApp
                                          mempty
                                          (MyVar mempty "f")
                                          (MyVar mempty "b")
                                      )
                                  )
                              ),
                              ( "These",
                                MyLambda
                                  mempty
                                  "a"
                                  ( MyLambda
                                      mempty
                                      "b"
                                      ( MyConsApp
                                          mempty
                                          ( MyConsApp
                                              mempty
                                              (MyConstructor mempty "These")
                                              (MyVar mempty "a")
                                          )
                                          ( MyApp
                                              mempty
                                              (MyVar mempty "f")
                                              (MyVar mempty "b")
                                          )
                                      )
                                  )
                              ),
                              ( "This",
                                MyLambda
                                  mempty
                                  "a"
                                  ( MyConsApp
                                      mempty
                                      (MyConstructor mempty "This")
                                      (MyVar mempty "a")
                                  )
                              )
                            ]
                        )
                        Nothing
                    )
                )
            )
    describe "typeclassMatches" $ do
      it "No instances for Void" $ do
        typeclassMatches dtVoid `shouldBe` mempty
      it "Enum instance for TrafficLights" $ do
        typeclassMatches dtTrafficLights `shouldSatisfy` elem Enum
      it "No enum instance for WrappedString" $ do
        typeclassMatches dtWrappedString `shouldNotSatisfy` elem Enum
      it "Newtype instance for WrappedString" $ do
        typeclassMatches dtWrappedString `shouldSatisfy` elem Newtype
      it "Functor instance for Identity" $ do
        typeclassMatches dtIdentity `shouldSatisfy` elem Functor
      it "Newtype instance for Identity" $ do
        typeclassMatches dtIdentity `shouldSatisfy` elem Newtype
      it "Functor instance for Maybe" $ do
        typeclassMatches dtMaybe `shouldSatisfy` elem Functor
      it "No newtype instance for Maybe" $ do
        typeclassMatches dtMaybe `shouldNotSatisfy` elem Newtype
