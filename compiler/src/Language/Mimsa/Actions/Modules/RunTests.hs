{-# LANGUAGE LambdaCase #-}

module Language.Mimsa.Actions.Modules.RunTests (runModuleTests) where

import Control.Monad.Except
import Data.Bifunctor
import Data.Map.Strict (Map)
import qualified Language.Mimsa.Actions.Modules.Evaluate as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Core
import Language.Mimsa.Tests.Helpers
import Language.Mimsa.Tests.UnitTest
import Language.Mimsa.Typechecker.Elaborate
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Tests

filterTests :: Map DefIdentifier (Expr Name ann) -> Map TestName (Expr Name ann)
filterTests =
  filterMapKeys
    ( \case
        (DITest tn) -> Just tn
        _ -> Nothing
    )

-- | run unit tests
-- although we do not take advantage of the typechecked module
-- we specify it to make sure we only work with non-broken modules
runModuleTests ::
  Module (Type Annotation) ->
  Actions.ActionM ModuleTestResults
runModuleTests mod' =
  let untypedModule = getAnnotationForType <$> mod'
   in ModuleTestResults <$> traverse (runUnitTest untypedModule) (filterTests (moExpressions mod'))

-- check the type of the unit test expression `Boolean`
-- explode if not
unifiesWithBoolean :: Expr Name MonoType -> Actions.ActionM ()
unifiesWithBoolean testExpr =
  void $
    liftEither $
      first (TypeErr (prettyPrint testExpr)) $
        resultIsBoolean (getTypeFromAnn testExpr)

runUnitTest :: Module Annotation -> Expr Name MonoType -> Actions.ActionM ModuleTestResult
runUnitTest mod' testExpr = do
  _ <- unifiesWithBoolean testExpr
  let untypedExpr = getAnnotationForType <$> testExpr
  (_, result, _) <- Actions.evaluateModule untypedExpr mod'
  pure $
    if testIsSuccess result
      then ModuleTestPassed
      else ModuleTestFailed
