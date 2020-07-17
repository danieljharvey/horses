{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main
  ( main,
  )
where

-- import qualified Data.Aeson as JSON
import qualified Test.DepGraph as DepGraph
import Test.Hspec
import qualified Test.Instantiate as Instantiate
import qualified Test.Interpreter as Interpreter
import Test.QuickCheck.Instances ()
import qualified Test.Repl as Repl
import qualified Test.Resolver as Resolver
import qualified Test.Substitutor as Substitutor
import qualified Test.Syntax as Syntax
import qualified Test.Typechecker as Typechecker
import qualified Test.Unify as Unify

main :: IO ()
main = hspec $ do
  Syntax.spec
  DepGraph.spec
  Interpreter.spec
  Resolver.spec
  Substitutor.spec
  Typechecker.spec
  Repl.spec
  Unify.spec
  Instantiate.spec
