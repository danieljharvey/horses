{-# LANGUAGE LambdaCase #-}

module Language.Mimsa.Actions.RunModuleTests (runModuleTests) where

import Control.Monad.Except
import Data.Bifunctor
import Data.Map (Map)
import qualified Language.Mimsa.Actions.Evaluate as Actions
import qualified Language.Mimsa.Actions.Monad as Actions
import Language.Mimsa.Printer
import Language.Mimsa.Tests.Helpers
import Language.Mimsa.Tests.UnitTest
import Language.Mimsa.Typechecker.Elaborate
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Modules
import Language.Mimsa.Types.Tests
import Language.Mimsa.Types.Typechecker
import Language.Mimsa.Utils

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
  Actions.ActionM (Map TestName ModuleTestResult)
runModuleTests mod' =
  let untypedModule = getAnnotationForType <$> mod'
   in traverse (runUnitTest untypedModule) (filterTests (moExpressions mod'))

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
  (_, result, _) <- Actions.evaluateModule (prettyPrint untypedExpr) untypedExpr mod'
  pure $
    if testIsSuccess result
      then ModuleTestPassed
      else ModuleTestFailed
