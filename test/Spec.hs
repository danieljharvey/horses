{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main
  ( main,
  )
where

import qualified Test.Actions as Actions
import qualified Test.BackendJS as JS
import Test.Hspec
import qualified Test.Interpreter.InstantiateVar as InstantiateVar
import qualified Test.Interpreter.Interpreter as Interpreter
import qualified Test.Interpreter.Repl as Repl
import qualified Test.Parser.MonoTypeParser as MonoTypeParser
import qualified Test.Parser.Syntax as Syntax
import qualified Test.Prettier as Prettier
import qualified Test.Project.NormaliseType as NormaliseType
import qualified Test.Project.TypeSearch as TypeSearch
import qualified Test.Project.UnitTest as UnitTest
import qualified Test.Project.Usages as Usages
import qualified Test.Serialisation as Serialisation
import qualified Test.Store.Resolver as Resolver
import qualified Test.Store.Substitutor as Substitutor
import qualified Test.Store.UpdateDeps as UpdateDeps
import qualified Test.Typechecker.Codegen as Codegen
import qualified Test.Typechecker.RecordUsage as RecordUsage
import qualified Test.Typechecker.TypeError as TypeError
import qualified Test.Typechecker.Typechecker as Typechecker
import qualified Test.Typechecker.Unify as Unify

main :: IO ()
main =
  hspec $ do
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
    Actions.spec
    Codegen.spec
