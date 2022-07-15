module Main
  ( main,
  )
where

import qualified Test.Actions.AddUnitTest as AddUnitTest
import qualified Test.Actions.BindExpression as BindExpression
import qualified Test.Actions.BindModule as BindModule
import qualified Test.Actions.BindType as BindType
import qualified Test.Actions.Build as Build
import qualified Test.Actions.Compile as Compile
import qualified Test.Actions.Evaluate as Evaluate
import qualified Test.Actions.Optimise as Optimise
import qualified Test.Actions.RemoveBinding as RemoveBinding
import qualified Test.Actions.Typecheck as TypecheckAction
import qualified Test.Actions.Upgrade as Upgrade
import qualified Test.Backend.ESModulesJS as ESModulesJS
import qualified Test.Backend.RunNode as RunNode
import qualified Test.Backend.Typescript as Typescript
import qualified Test.Codegen as Codegen
import Test.Hspec
import qualified Test.Interpreter.Repl as Repl
import qualified Test.Modules.Check as ModuleCheck
import qualified Test.Modules.Compile as ModuleCompile
import qualified Test.Modules.Repl as ModuleRepl
import qualified Test.Modules.Test as ModuleTest
import qualified Test.Parser.MonoTypeParser as MonoTypeParser
import qualified Test.Parser.Pattern as Pattern
import qualified Test.Parser.Syntax as Syntax
import qualified Test.Prettier as Prettier
import qualified Test.Project.NormaliseType as NormaliseType
import qualified Test.Project.SourceSpan as SourceSpan
import qualified Test.Project.Stdlib as Stdlib
import qualified Test.Project.TypeSearch as TypeSearch
import qualified Test.Project.Usages as Usages
import qualified Test.RenderErrors as RenderErrors
import qualified Test.Serialisation as Serialisation
import qualified Test.Store.Resolver as Resolver
import qualified Test.Store.UpdateDeps as UpdateDeps
import qualified Test.Tests.Properties as Properties
import qualified Test.Tests.PropertyTest as PropertyTest
import qualified Test.Tests.UnitTest as UnitTest
import qualified Test.Transform.BetaReduce as BetaReduce
import qualified Test.Transform.EtaReduce as EtaReduce
import qualified Test.Transform.FindUnused as FindUnused
import qualified Test.Transform.FindUses as FindUses
import qualified Test.Transform.FlattenLets as FlattenLets
import qualified Test.Transform.FloatDown as FloatDown
import qualified Test.Transform.FloatUp as FloatUp
import qualified Test.Transform.Inliner as Inliner
import qualified Test.Transform.SimplifyPatterns as SimplifyPatterns
import qualified Test.Typechecker.DataTypes as DataTypes
import qualified Test.Typechecker.Elaborate as Elaborate
import qualified Test.Typechecker.Exhaustiveness as Exhaustiveness
import qualified Test.Typechecker.NumberVars as NumberVars
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
    Prettier.spec
    Resolver.spec
    Repl.spec
    Unify.spec
    Usages.spec
    TypeError.spec
    Serialisation.spec
    NormaliseType.spec
    TypeSearch.spec
    MonoTypeParser.spec
    UnitTest.spec
    PropertyTest.spec
    UpdateDeps.spec
    BindType.spec
    Codegen.spec
    AddUnitTest.spec
    BindExpression.spec
    BindModule.spec
    Compile.spec
    Evaluate.spec
    Upgrade.spec
    Stdlib.spec
    Exhaustiveness.spec
    Pattern.spec
    RemoveBinding.spec
    Typecheck.spec
    RunNode.spec
    DataTypes.spec
    Elaborate.spec
    SourceSpan.spec
    OutputTypes.spec
    Typescript.spec
    ESModulesJS.spec
    Substitutions.spec
    ScopeTypeVar.spec
    Properties.spec
    FindUnused.spec
    FlattenLets.spec
    Optimise.spec
    FloatDown.spec
    FloatUp.spec
    Inliner.spec
    FindUses.spec
    BetaReduce.spec
    EtaReduce.spec
    Build.spec
    TypecheckAction.spec
    SimplifyPatterns.spec
    NumberVars.spec
    ModuleCheck.spec
    RenderErrors.spec
    ModuleRepl.spec
    ModuleCompile.spec
    ModuleTest.spec
