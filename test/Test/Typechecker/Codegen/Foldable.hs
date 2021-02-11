{-# LANGUAGE OverloadedStrings #-}

module Test.Typechecker.Codegen.Foldable
  ( spec,
  )
where

import Data.Either (isRight)
import qualified Data.List.NonEmpty as NE
import Language.Mimsa.Typechecker.Codegen
import Language.Mimsa.Types.AST
import Test.Hspec
import Test.Typechecker.Codegen.Shared

spec :: Spec
spec = do
  describe "Foldable instances" $ do
    it "Generates fold for dtIdentity" $ do
      typecheckInstance fold dtIdentity `shouldSatisfy` isRight
      fold dtIdentity
        `shouldBe` Right
          ( MyLet
              mempty
              "fold"
              ( MyLambda
                  mempty
                  "f"
                  ( MyLambda
                      mempty
                      "total"
                      ( MyLambda
                          mempty
                          "identity"
                          ( MyCaseMatch
                              mempty
                              (MyVar mempty "identity")
                              ( NE.fromList
                                  [ ( "Identity",
                                      MyLambda
                                        mempty
                                        "a"
                                        ( MyApp
                                            mempty
                                            (MyApp mempty (MyVar mempty "f") (MyVar mempty "total"))
                                            (MyVar mempty "a")
                                        )
                                    )
                                  ]
                              )
                              Nothing
                          )
                      )
                  )
              )
              (MyVar mempty "fold")
          )
    it "Generates fold for dtMaybe" $ do
      typecheckInstance fold dtMaybe `shouldSatisfy` isRight
      fold dtMaybe
        `shouldBe` Right
          ( MyLet
              mempty
              "fold"
              ( MyLambda
                  mempty
                  "f"
                  ( MyLambda
                      mempty
                      "total"
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
                                        ( MyApp
                                            mempty
                                            (MyApp mempty (MyVar mempty "f") (MyVar mempty "total"))
                                            (MyVar mempty "a")
                                        )
                                    ),
                                    ("Nothing", MyVar mempty "total")
                                  ]
                              )
                              Nothing
                          )
                      )
                  )
              )
              (MyVar mempty "fold")
          )
    it "Generates fold for dtThese" $ do
      typecheckInstance fold dtThese `shouldSatisfy` isRight
      fold dtThese
        `shouldBe` Right
          ( MyLet
              mempty
              "fold"
              ( MyLambda
                  mempty
                  "f"
                  ( MyLambda
                      mempty
                      "total"
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
                                        ( MyApp
                                            mempty
                                            (MyApp mempty (MyVar mempty "f") (MyVar mempty "total"))
                                            (MyVar mempty "b")
                                        )
                                    ),
                                    ( "These",
                                      MyLambda
                                        mempty
                                        "a"
                                        ( MyLambda
                                            mempty
                                            "b"
                                            ( MyApp
                                                mempty
                                                (MyApp mempty (MyVar mempty "f") (MyVar mempty "total"))
                                                (MyVar mempty "b")
                                            )
                                        )
                                    ),
                                    ( "This",
                                      MyLambda
                                        mempty
                                        "a"
                                        (MyVar mempty "total")
                                    )
                                  ]
                              )
                              Nothing
                          )
                      )
                  )
              )
              (MyVar mempty "fold")
          )
    it "Generates fold for dtList" $ do
      typecheckInstance fold dtList `shouldSatisfy` isRight
      fold dtList
        `shouldBe` Right
          ( MyLet
              mempty
              "fold"
              ( MyLambda
                  mempty
                  "f"
                  ( MyLambda
                      mempty
                      "total"
                      ( MyLambda
                          mempty
                          "list"
                          ( MyCaseMatch
                              mempty
                              (MyVar mempty "list")
                              ( NE.fromList
                                  [ ( "Cons",
                                      MyLambda
                                        mempty
                                        "a"
                                        ( MyLambda
                                            mempty
                                            "list1"
                                            ( MyApp
                                                mempty
                                                ( MyApp
                                                    mempty
                                                    ( MyApp
                                                        mempty
                                                        ( MyVar
                                                            mempty
                                                            "fold"
                                                        )
                                                        (MyVar mempty "f")
                                                    )
                                                    ( MyApp
                                                        mempty
                                                        ( MyApp
                                                            mempty
                                                            (MyVar mempty "f")
                                                            (MyVar mempty "total")
                                                        )
                                                        (MyVar mempty "a")
                                                    )
                                                )
                                                (MyVar mempty "list1")
                                            )
                                        )
                                    ),
                                    ( "Nil",
                                      MyVar mempty "total"
                                    )
                                  ]
                              )
                              Nothing
                          )
                      )
                  )
              )
              (MyVar mempty "fold")
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
