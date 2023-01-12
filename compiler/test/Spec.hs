module Main
  ( main,
  )
where

import qualified Test.Actions.BindModule
import qualified Test.Actions.Build
import qualified Test.Actions.Compile
import qualified Test.Actions.Evaluate
import qualified Test.Backend.ESModulesJSEndToEnd
import qualified Test.Backend.RunNode
import qualified Test.Backend.TypescriptEndToEnd
import qualified Test.Backend.Wasm
import Test.Hspec
import qualified Test.Modules.Check
import qualified Test.Modules.Repl
import qualified Test.Modules.Test
import qualified Test.Modules.ToStoreExprs
import qualified Test.Modules.Uses
import qualified Test.Project.NormaliseType
import qualified Test.Project.SourceSpan
import qualified Test.Project.Stdlib
import qualified Test.RenderErrors
import qualified Test.Serialisation
import qualified Test.Tests.Properties
import qualified Test.Transform.BetaReduce
import qualified Test.Transform.EtaReduce
import qualified Test.Transform.FindUnused
import qualified Test.Transform.FindUses
import qualified Test.Transform.FlattenLets
import qualified Test.Transform.FloatDown
import qualified Test.Transform.FloatUp
import qualified Test.Transform.Inliner
import qualified Test.Transform.SimplifyPatterns
import qualified Test.Typechecker.DataTypes
import qualified Test.Typechecker.Elaborate
import qualified Test.Typechecker.Exhaustiveness
import qualified Test.Typechecker.NumberVars
import qualified Test.Typechecker.ScopeTypeVar
import qualified Test.Typechecker.Substitutions
import qualified Test.Typechecker.Typecheck
import qualified Test.Typechecker.Unify

main :: IO ()
main =
  hspec $ do
    Test.Actions.BindModule.spec
    Test.Actions.Build.spec
    Test.Actions.Compile.spec
    Test.Actions.Evaluate.spec
    Test.Backend.ESModulesJSEndToEnd.spec
    Test.Backend.RunNode.spec
    Test.Backend.TypescriptEndToEnd.spec
    Test.Backend.Wasm.spec
    Test.Modules.Check.spec
    Test.Modules.Repl.spec
    Test.Modules.Test.spec
    Test.Modules.ToStoreExprs.spec
    Test.Modules.Uses.spec
    Test.Project.NormaliseType.spec
    Test.Project.SourceSpan.spec
    Test.Project.Stdlib.spec
    Test.RenderErrors.spec
    Test.Serialisation.spec
    Test.Tests.Properties.spec
    Test.Transform.BetaReduce.spec
    Test.Transform.EtaReduce.spec
    Test.Transform.FindUnused.spec
    Test.Transform.FindUses.spec
    Test.Transform.FlattenLets.spec
    Test.Transform.FloatDown.spec
    Test.Transform.FloatUp.spec
    Test.Transform.Inliner.spec
    Test.Transform.SimplifyPatterns.spec
    Test.Typechecker.DataTypes.spec
    Test.Typechecker.Elaborate.spec
    Test.Typechecker.Exhaustiveness.spec
    Test.Typechecker.NumberVars.spec
    Test.Typechecker.ScopeTypeVar.spec
    Test.Typechecker.Substitutions.spec
    Test.Typechecker.Typecheck.spec
    Test.Typechecker.Unify.spec
