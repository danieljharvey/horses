{-# LANGUAGE OverloadedStrings #-}

module Test.Typechecker.Codegen.Functor
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
  describe "Functor instances" $ do
    it "Generates functorMap for dtIdentity" $ do
      typecheckInstance functorMap dtIdentity `shouldSatisfy` isRight
      functorMap dtIdentity
        `shouldBe` Right
          ( MyLet
              mempty
              "fmap"
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
              (MyVar mempty "fmap")
          )
    it "Generates functorMap for dtMaybe" $ do
      typecheckInstance functorMap dtMaybe `shouldSatisfy` isRight
      functorMap dtMaybe
        `shouldBe` Right
          ( MyLet
              mempty
              "fmap"
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
              (MyVar mempty "fmap")
          )
    it "Generates functorMap for dtThese" $ do
      typecheckInstance functorMap dtThese `shouldSatisfy` isRight
      functorMap dtThese
        `shouldBe` Right
          ( MyLet
              mempty
              "fmap"
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
              (MyVar mempty "fmap")
          )
    it "Generates functorMap for dtList" $ do
      typecheckInstance functorMap dtList `shouldSatisfy` isRight
      functorMap dtList
        `shouldBe` Right
          ( MyLet
              mempty
              "fmap"
              ( MyLambda
                  mempty
                  "f"
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
                                        ( MyConsApp
                                            mempty
                                            ( MyConsApp
                                                mempty
                                                (MyConstructor mempty "Cons")
                                                ( MyApp
                                                    mempty
                                                    (MyVar mempty "f")
                                                    (MyVar mempty "a")
                                                )
                                            )
                                            ( MyApp
                                                mempty
                                                ( MyApp
                                                    mempty
                                                    (MyVar mempty "fmap")
                                                    (MyVar mempty "f")
                                                )
                                                (MyVar mempty "list1")
                                            )
                                        )
                                    )
                                ),
                                ( "Nil",
                                  MyConstructor mempty "Nil"
                                )
                              ]
                          )
                          Nothing
                      )
                  )
              )
              (MyVar mempty "fmap")
          )
    it "Generates functorMap for dtDoubleList" $ do
      typecheckInstance functorMap dtDoubleList `shouldSatisfy` isRight
      functorMap dtDoubleList
        `shouldBe` Right
          ( MyLet
              mempty
              "fmap"
              ( MyLambda
                  mempty
                  "f"
                  ( MyLambda
                      mempty
                      "doubleList"
                      ( MyCaseMatch
                          mempty
                          (MyVar mempty "doubleList")
                          ( NE.fromList
                              [ ( "DoubleCons",
                                  MyLambda
                                    mempty
                                    "a"
                                    ( MyLambda
                                        mempty
                                        "b"
                                        ( MyLambda
                                            mempty
                                            "doubleList1"
                                            ( MyConsApp
                                                mempty
                                                ( MyConsApp
                                                    mempty
                                                    ( MyConsApp
                                                        mempty
                                                        (MyConstructor mempty "DoubleCons")
                                                        (MyVar mempty "a")
                                                    )
                                                    (MyApp mempty (MyVar mempty "f") (MyVar mempty "b"))
                                                )
                                                ( MyApp
                                                    mempty
                                                    ( MyApp
                                                        mempty
                                                        (MyVar mempty "fmap")
                                                        (MyVar mempty "f")
                                                    )
                                                    (MyVar mempty "doubleList1")
                                                )
                                            )
                                        )
                                    )
                                ),
                                ( "DoubleNil",
                                  MyConstructor mempty "DoubleNil"
                                )
                              ]
                          )
                          Nothing
                      )
                  )
              )
              (MyVar mempty "fmap")
          )
    it "Generates functorMap for dtTree" $ do
      typecheckInstance functorMap dtTree `shouldSatisfy` isRight
      functorMap dtTree
        `shouldBe` Right
          ( MyLet
              mempty
              "fmap"
              ( MyLambda
                  mempty
                  "f"
                  ( MyLambda
                      mempty
                      "tree"
                      ( MyCaseMatch
                          mempty
                          (MyVar mempty "tree")
                          ( NE.fromList
                              [ ( "Branch",
                                  MyLambda
                                    mempty
                                    "tree1"
                                    ( MyLambda
                                        mempty
                                        "tree2"
                                        ( MyConsApp
                                            mempty
                                            ( MyConsApp
                                                mempty
                                                (MyConstructor mempty "Branch")
                                                ( MyApp
                                                    mempty
                                                    ( MyApp
                                                        mempty
                                                        (MyVar mempty "fmap")
                                                        (MyVar mempty "f")
                                                    )
                                                    (MyVar mempty "tree1")
                                                )
                                            )
                                            ( MyApp
                                                mempty
                                                ( MyApp
                                                    mempty
                                                    (MyVar mempty "fmap")
                                                    (MyVar mempty "f")
                                                )
                                                (MyVar mempty "tree2")
                                            )
                                        )
                                    )
                                ),
                                ( "Leaf",
                                  MyLambda
                                    mempty
                                    "a"
                                    ( MyConsApp
                                        mempty
                                        (MyConstructor mempty "Leaf")
                                        ( MyApp
                                            mempty
                                            (MyVar mempty "f")
                                            (MyVar mempty "a")
                                        )
                                    )
                                )
                              ]
                          )
                          Nothing
                      )
                  )
              )
              (MyVar mempty "fmap")
          )
    it "Generates functorMap for dtReader" $ do
      typecheckInstance functorMap dtReader `shouldSatisfy` isRight
      functorMap dtReader
        `shouldBe` Right
          ( MyLet
              mempty
              "fmap"
              ( MyLambda
                  mempty
                  "f"
                  ( MyLambda
                      mempty
                      "reader"
                      ( MyCaseMatch
                          mempty
                          (MyVar mempty "reader")
                          ( pure
                              ( "Reader",
                                MyLambda
                                  mempty
                                  "rtoa"
                                  ( MyConsApp
                                      mempty
                                      (MyConstructor mempty "Reader")
                                      ( MyLambda
                                          mempty
                                          "r"
                                          ( MyApp
                                              mempty
                                              (MyVar mempty "f")
                                              ( MyApp
                                                  mempty
                                                  (MyVar mempty "rtoa")
                                                  (MyVar mempty "r")
                                              )
                                          )
                                      )
                                  )
                              )
                          )
                          Nothing
                      )
                  )
              )
              (MyVar mempty "fmap")
          )
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
