{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main
  ( main,
  )
where

import qualified Test.BackendJS as JS
import Test.Hspec
import qualified Test.InstantiateVar as InstantiateVar
import qualified Test.Interpreter as Interpreter
import qualified Test.MonoTypeParser as MonoTypeParser
import qualified Test.NormaliseType as NormaliseType
import qualified Test.Prettier as Prettier
import Test.QuickCheck.Instances ()
import qualified Test.RecordUsage as RecordUsage
import qualified Test.Repl as Repl
import qualified Test.Resolver as Resolver
import qualified Test.Serialisation as Serialisation
import qualified Test.Substitutor as Substitutor
import qualified Test.Syntax as Syntax
import qualified Test.TypeError as TypeError
import qualified Test.TypeSearch as TypeSearch
import qualified Test.Typechecker as Typechecker
import qualified Test.Unify as Unify
import qualified Test.UnitTest as UnitTest
import qualified Test.UpdateDeps as UpdateDeps
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
  TypeError.spec
  Serialisation.spec
  RecordUsage.spec
  NormaliseType.spec
  TypeSearch.spec
  MonoTypeParser.spec
  InstantiateVar.spec
  UnitTest.spec
  UpdateDeps.spec
