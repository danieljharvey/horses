{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Modules.Check (checkModule, exprAndTypeFromParts) where

import Control.Monad.Except
import Data.Bifunctor
import Data.Coerce
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Debug.Trace
import qualified Language.Mimsa.Actions.Helpers.Build as Build
import Language.Mimsa.Parser.Module
import Language.Mimsa.Store
import Language.Mimsa.Typechecker.DataTypes
import Language.Mimsa.Typechecker.Elaborate
import Language.Mimsa.Typechecker.NumberVars
import Language.Mimsa.Typechecker.Typecheck
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Identifiers.TypeName
import Language.Mimsa.Types.Typechecker

-- | This is where we load a file and check that it is "OK" as such
--  so far this entails:
--  1. parsing it
--  2. ordering things
--  3. typechecking everything
--
--  so far the features in modules are
--  1. definitions of values
--  2. types of values
--  3. definitions of datatypes
--
--  soon there will also need to be
--  1. infix definitions
--  2. imports
--  3. exports
checkModule :: Text -> Either (Error Annotation) (Module Annotation)
checkModule input = do
  moduleItems <- first (ParseError input) (parseModule input)
  properMod <- first ModuleErr (moduleFromModuleParts moduleItems)
  _deps <- first ModuleErr (typecheckAll properMod)
  pure properMod

-- get the vars used by each def
-- explode if there's not available
-- this will need updating to include imports when we implement them
getValueDependencies ::
  (Eq ann, Monoid ann) =>
  Module ann ->
  Either
    ModuleError
    ( Map
        Name
        ( Maybe (Type ann),
          Expr Name ann,
          Set Name
        )
    )
getValueDependencies mod' = do
  let check (mt, exp') =
        let deps = extractVars exp'
            unknownDeps = S.filter (\dep -> S.notMember dep (M.keysSet (moExpressions mod'))) deps
         in if S.null unknownDeps
              then Right (mt, exp', traceShowId deps)
              else throwError (CannotFindValues unknownDeps)
  traverse check (moExpressions mod')

typecheckAll :: Module Annotation -> Either ModuleError (Map Name (Expr Name MonoType))
typecheckAll inputModule = do
  -- create initial state for builder
  -- we tag each StoreExpression we've found with the deps it needs
  inputWithDeps <- getValueDependencies inputModule
  let inputWithDepsAndName = M.mapWithKey (,) inputWithDeps

  let state =
        Build.State
          { Build.stInputs =
              ( \(name, (mt, expr, deps)) ->
                  Build.Plan
                    { Build.jbDeps = deps,
                      Build.jbInput = (name, mt, expr)
                    }
              )
                <$> inputWithDepsAndName,
            Build.stOutputs = mempty
          }
  -- go!
  Build.stOutputs
    <$> Build.doJobs (typecheckOne inputModule) state

makeTypeDeclMap :: Module ann -> Map TyCon DataType
makeTypeDeclMap inputModule =
  M.fromList . fmap (first coerce) . M.toList $ moDataTypes inputModule

typecheckOne ::
  Module Annotation ->
  Map Name (Expr Name MonoType) ->
  (Name, Maybe MonoType, Expr Name Annotation) ->
  Either ModuleError (Expr Name MonoType)
typecheckOne inputModule deps (name, _mt, expr) = do
  let typeMap = getTypeFromAnn <$> deps
  -- number the vars
  numberedExpr <-
    first
      (DefDoesNotTypeCheck name)
      (addNumbersToExpression (M.keysSet deps) expr)
  -- initial typechecking environment
  let env = traceShowId $ createEnv (getTypeFromAnn <$> deps) (makeTypeDeclMap inputModule)
  -- typecheck it
  (_subs, _constraints, typedExpr, _mt) <-
    first
      (DefDoesNotTypeCheck name)
      (typecheck typeMap env numberedExpr)
  pure (first fst typedExpr)

moduleFromModuleParts :: (Monoid ann) => [ModuleItem ann] -> Either ModuleError (Module ann)
moduleFromModuleParts parts =
  let addPart part = \case
        Left e -> Left e
        Right mod' -> case part of
          ModuleExpression name bits expr ->
            case M.lookup name (moExpressions mod') of
              Just _ -> throwError (DuplicateDefinition name)
              Nothing ->
                let (exp', mt) = exprAndTypeFromParts bits expr
                 in pure $
                      mod'
                        { moExpressions =
                            M.singleton name (mt, exp') <> moExpressions mod'
                        }
          ModuleDataType dt@(DataType tyCon _ _) ->
            let typeName = coerce tyCon
             in case M.lookup typeName (moDataTypes mod') of
                  Just _ -> throwError (DuplicateTypeName typeName)
                  Nothing ->
                    pure $
                      mod'
                        { moDataTypes =
                            M.singleton typeName dt
                              <> moDataTypes mod'
                        }
   in foldr addPart (Right mempty) parts

-- given the bits of things, make a coherent type and expression
exprAndTypeFromParts ::
  (Monoid ann) =>
  [DefPart ann] ->
  Expr Name ann ->
  (Expr Name ann, Maybe (Type ann))
exprAndTypeFromParts parts expr =
  let expr' =
        foldr
          ( \part rest -> case part of
              (DefArg ident) -> MyLambda mempty ident rest
              (DefTypedArg ident _) -> MyLambda mempty ident rest
              (DefType _) -> rest
          )
          expr
          parts
      -- if we only have un-typed args, don't bother, we only want them as
      -- placeholders
      filteredParts =
        let includesExplicitTypes =
              any
                ( \case
                    (DefArg _) -> False
                    _ -> True
                )
                parts
            includesReturnType =
              any
                ( \case
                    (DefType _) -> True
                    _ -> False
                )
                parts
         in if includesExplicitTypes
              then
                if includesReturnType
                  then parts
                  else parts <> [DefType (MTVar mempty (TVName "returnType"))]
              else mempty
      mt =
        foldr
          ( \part rest -> case part of
              (DefArg (Identifier _ name)) -> case rest of
                Just rest' ->
                  Just
                    ( MTFunction
                        mempty
                        (MTVar mempty (TVName (coerce name)))
                        rest'
                    )
                Nothing ->
                  Just
                    ( MTVar
                        mempty
                        (TVName (coerce name))
                    )
              (DefTypedArg _ thisMt) -> case rest of
                Just rest' ->
                  Just
                    (MTFunction mempty thisMt rest')
                _ -> Just thisMt
              (DefType thisMt) -> case rest of
                Just rest' ->
                  Just
                    (MTFunction mempty rest' thisMt)
                _ -> Just thisMt
          )
          Nothing
          filteredParts
   in (expr', mt)
