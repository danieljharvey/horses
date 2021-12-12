module Main
  ( main,
  )
where

import qualified Test.Actions.AddUnitTest as AddUnitTest
import qualified Test.Actions.BindExpression as BindExpression
import qualified Test.Actions.BindType as BindType
import qualified Test.Actions.Compile as Compile
import qualified Test.Actions.Evaluate as Evaluate
import qualified Test.Actions.RemoveBinding as RemoveBinding
import qualified Test.Backend.ESModulesJS as ESModulesJS
import qualified Test.Backend.RunNode as RunNode
import qualified Test.Backend.Runtimes as Runtimes
import qualified Test.Backend.Typescript as Typescript
import qualified Test.Codegen as Codegen
import Test.Hspec
import qualified Test.Interpreter.InstantiateVar as InstantiateVar
import qualified Test.Interpreter.Interpreter as Interpreter
import qualified Test.Interpreter.Repl as Repl
import qualified Test.Interpreter.UseSwaps as UseSwaps
import qualified Test.Parser.MonoTypeParser as MonoTypeParser
import qualified Test.Parser.Pattern as Pattern
import qualified Test.Parser.Syntax as Syntax
import qualified Test.Prettier as Prettier
import qualified Test.Project.NormaliseType as NormaliseType
import qualified Test.Project.SourceSpan as SourceSpan
import qualified Test.Project.Stdlib as Stdlib
import qualified Test.Project.TypeSearch as TypeSearch
import qualified Test.Project.Usages as Usages
import qualified Test.Serialisation as Serialisation
import qualified Test.Store.Resolver as Resolver
import qualified Test.Store.Substitutor as Substitutor
import qualified Test.Store.UpdateDeps as UpdateDeps
import qualified Test.Tests.Properties as Properties
import qualified Test.Tests.UnitTest as UnitTest
import qualified Test.Typechecker.DataTypes as DataTypes
import qualified Test.Typechecker.Elaborate as Elaborate
import qualified Test.Typechecker.Exhaustiveness as Exhaustiveness
import qualified Test.Typechecker.KindChecker as KindChecker
import qualified Test.Typechecker.OutputTypes as OutputTypes
import qualified Test.Typechecker.ScopeTypeVar as ScopeTypeVar
import qualified Test.Typechecker.Substitutions as Substitutions
import qualified Test.Typechecker.TypeError as TypeError
import qualified Test.Typechecker.Typecheck as Typecheck
import qualified Test.Typechecker.Unify as Unify

main :: IO ()
main =
  hspec $ do
    Syntax.spec
    Interpreter.spec
    Prettier.spec
    Resolver.spec
    Substitutor.spec
    Repl.spec
    Unify.spec
    Usages.spec
    TypeError.spec
    Serialisation.spec
    NormaliseType.spec
    TypeSearch.spec
    MonoTypeParser.spec
    InstantiateVar.spec
    UnitTest.spec
    UpdateDeps.spec
    BindType.spec
    Codegen.spec
    Runtimes.spec
    UseSwaps.spec
    AddUnitTest.spec
    BindExpression.spec
    Compile.spec
    Evaluate.spec
    Stdlib.spec
    Exhaustiveness.spec
    Pattern.spec
    RemoveBinding.spec
    Typecheck.spec
    RunNode.spec
    DataTypes.spec
    Elaborate.spec
    KindChecker.spec
    SourceSpan.spec
    OutputTypes.spec
    Typescript.spec
    ESModulesJS.spec
    Substitutions.spec
    ScopeTypeVar.spec
    Properties.spec
