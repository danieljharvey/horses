{-# LANGUAGE OverloadedStrings #-}

module Test.Typechecker.Codegen.Newtype
  ( spec,
  )
where

import Data.Either (isRight)
import Language.Mimsa.Typechecker.Codegen
import Language.Mimsa.Types.AST
import Test.Hspec
import Test.Typechecker.Codegen.Shared

spec :: Spec
spec = do
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
