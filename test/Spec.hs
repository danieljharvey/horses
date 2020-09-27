{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main
  ( main,
  )
where

-- import qualified Data.Aeson as JSON
import qualified Test.BackendJS as JS
import Test.Hspec
import qualified Test.Interpreter as Interpreter
import qualified Test.Prettier as Prettier
import Test.QuickCheck.Instances ()
import qualified Test.Repl as Repl
import qualified Test.Resolver as Resolver
import qualified Test.Substitutor as Substitutor
import qualified Test.Syntax as Syntax
import qualified Test.Typechecker as Typechecker
import qualified Test.Unify as Unify
import qualified Test.Usages as Usages

main :: IO ()
main = hspec $ do
  Syntax.spec
  Interpreter.spec
  Prettier.spec
  Resolver.spec
  Substitutor.spec
  Typechecker.spec
  Repl.spec
  Unify.spec
  Usages.spec
  JS.spec
