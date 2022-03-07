{-# LANGUAGE OverloadedStrings #-}

module Test.Backend.Wasm
  ( spec,
  )
where

import Control.Monad.Except
import Data.Bifunctor
import Data.Foldable
import Data.Functor
import Data.Hashable
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.Mimsa.Actions.Shared as Actions
import Language.Mimsa.Backend.Types
import Language.Mimsa.Backend.Typescript.DataType
import Language.Mimsa.Backend.Typescript.FromExpr
import Language.Mimsa.Backend.Typescript.Monad
import Language.Mimsa.Backend.Typescript.Patterns
import Language.Mimsa.Backend.Typescript.Printer
import Language.Mimsa.Backend.Typescript.Types
import Language.Mimsa.Backend.Wasm
import Language.Mimsa.Interpreter.UseSwaps
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Typechecker
import Test.Backend.RunNode hiding (spec)
import Test.Data.Project
import Test.Hspec
import Test.Utils.Compilation
import Test.Utils.Helpers
import Test.Utils.Serialisation

spec :: Spec
spec = do
  describe "Wasm" $ do
    it "literals" $ do
      printLiteral (TSBool True) `shouldBe` "true"
      printLiteral (TSInt 100) `shouldBe` "100"
      printLiteral (TSString "egg") `shouldBe` "\"egg\""
