{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Backend.Typescript.FromExpr where

import Control.Monad.Except
import Control.Monad.Writer
import Data.Coerce (coerce)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Language.Mimsa.Backend.NormaliseConstructors
import Language.Mimsa.Backend.Typescript.Types
import Language.Mimsa.ExprUtils
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker

toLiteral :: Literal -> TSLiteral
toLiteral lit = case lit of
  (MyInt i) -> TSInt i
  (MyBool b) -> TSBool b
  (MyString (StringType s)) -> TSString s

toPattern :: Pattern Name ann -> TSPattern
toPattern (PVar _ a) = TSPatternVar a
toPattern (PPair _ a b) = TSPatternPair (toPattern a) (toPattern b)
toPattern (PWildcard _) = TSPatternWildcard
toPattern (PConstructor _ name vars) =
  TSPatternConstructor name (toPattern <$> vars)
toPattern _ = undefined

-- | returns the type and any generics used in the expression
toTSType :: Type ann -> (TSType, Set TSGeneric)
toTSType (MTPrim _ MTString) = (TSType "string" [], mempty)
toTSType (MTPrim _ MTInt) = (TSType "number" [], mempty)
toTSType (MTPrim _ MTBool) = (TSType "boolean" [], mempty)
toTSType (MTVar _ a) =
  let newVar = case a of
        TVNum i' -> T.toTitle (T.pack (printTypeNum (i' + 1)))
        TVName a' -> T.toTitle (coerce a')
   in (TSTypeVar newVar, S.singleton (TSGeneric newVar))
toTSType mt@MTTypeApp {} =
  case varsFromDataType mt of
    Just (TyCon n, vars) ->
      let (types, generics) = unzip (toTSType <$> vars)
       in (TSType n types, mconcat generics)
    Nothing ->
      (TSType "weird type app error" mempty, mempty)
toTSType (MTFunction _ a b) =
  let (tsA, genA) = toTSType a
      (tsB, genB) = toTSType b
   in (TSTypeFun "arg" tsA tsB, genA <> genB)
toTSType e = (TSType ("unknown for: " <> prettyPrint e) mempty, mempty)

toTSDataType :: DataType -> TSDataType
toTSDataType (DataType name gens cons) =
  TSDataType name (T.toTitle . prettyPrint <$> gens) (toTSCons <$> M.toList cons)
  where
    toTSCons (tyCon, con) = TSConstructor tyCon (fst . toTSType <$> con)

newGenerics :: Set TSGeneric -> Set TSGeneric -> Set TSGeneric
newGenerics old new = S.difference new old

-- | Write datatype declarations as we create them
type TypescriptM = ExceptT (BackendError MonoType) (Writer [TSDataType])

runTypescriptM :: TypescriptM a -> Either (BackendError MonoType) (a, [TSDataType])
runTypescriptM computation = case runWriter (runExceptT computation) of
  (Right a, dts) -> pure (a, dts)
  (Left e, _) -> throwError e

-- | TODO: what to do about normalise constructors?
-- do we require ResolvedTypeDeps like the other outputs?
-- this would still work once we stop cramming the entire universe into the
-- expr for typechecking etc
--
-- or we modify a version of normaliseConstructors that takes the new datatype
-- we find in a MyData, modifies all constructors into functions and leaves the
-- rest rather than erroring?
fromExpr :: Expr Name MonoType -> Either (BackendError MonoType) TSModule
fromExpr expr = do
  (result, dataTypes) <- runTypescriptM (makeTSExpr mempty expr)
  pure (TSModule dataTypes result)
  where
    makeTSExpr :: Set TSGeneric -> Expr Name MonoType -> TypescriptM TSBody
    makeTSExpr generics expr' =
      case expr' of
        (MyLiteral _ lit) ->
          pure $ TSBody mempty (TSLit (toLiteral lit))
        (MyLet _ name letExpr letBody) -> do
          newLetExpr <- makeTSExpr generics letExpr
          let newBinding =
                TSAssignment
                  (TSPatternVar name)
                  (TSLetBody newLetExpr)
          (TSBody bindings' newExpr) <- makeTSExpr generics letBody
          pure (TSBody ([newBinding] <> bindings') newExpr)
        (MyLetPattern _ pat letExpr letBody) -> do
          newLetExpr <- makeTSExpr generics letExpr
          let newBinding =
                TSAssignment
                  (toPattern pat)
                  (TSLetBody newLetExpr)
          (TSBody bindings' newExpr) <- makeTSExpr generics letBody
          pure (TSBody ([newBinding] <> bindings') newExpr)
        (MyPair _ a b) -> do
          (TSBody _ tsA) <- makeTSExpr generics a
          (TSBody _ tsB) <- makeTSExpr generics b
          pure (TSBody mempty (TSArray [tsA, tsB]))
        (MyVar _ a) -> pure (TSBody mempty (TSVar a))
        (MyLambda fnType bind body) -> do
          let (mtFn, generics') = toTSType fnType
              mtArg = case mtFn of
                (TSTypeFun _ a _) -> a
                _ -> error "function does not have function type"
              newGenerics' = newGenerics generics generics'
          tsBody <- makeTSExpr (generics <> generics') body
          pure $
            TSBody
              []
              ( TSFunction
                  bind
                  newGenerics'
                  mtArg
                  ( TSFunctionBody tsBody
                  )
              )
        (MyPatternMatch _mtPatternMatch matchExpr patterns) -> do
          matches <-
            traverse
              ( \(pat, patExpr) -> do
                  let tsPat = toPattern pat
                  (TSBody parts tsPatExpr) <- makeTSExpr generics patExpr
                  let item = TSAssignment tsPat (TSLetBody (TSBody [] (TSVar "value")))
                  pure $
                    TSConditional
                      (toPattern pat)
                      (TSLetBody (TSBody (item : parts) tsPatExpr))
              )
              patterns
          (TSBody _ tsA) <- makeTSExpr generics matchExpr
          let (tyMatchExpr, matchGenerics) = toTSType (getAnnotation matchExpr)
          pure $
            TSBody
              [ TSAssignment
                  (TSPatternVar "match")
                  ( TSLetBody
                      ( TSBody
                          []
                          ( TSFunction
                              "value"
                              (newGenerics generics matchGenerics)
                              tyMatchExpr
                              ( TSFunctionBody
                                  ( TSBody
                                      matches
                                      (TSError "Pattern match error")
                                  )
                              )
                          )
                      )
                  )
              ]
              (TSApp (TSVar "match") tsA)
        (MyApp _mtApp (MyConstructor _ consName) vals) -> do
          tsVals <-
            traverse
              (makeTSExpr generics)
              (getConsArgList vals)
          let tsVals' = (\(TSBody _ inner) -> inner) <$> tsVals
          pure $ TSBody [] (TSData (prettyPrint consName) tsVals')
        (MyApp _mtApp func val) -> do
          (TSBody _ tsFunc) <- makeTSExpr generics func
          (TSBody _ tsVal) <- makeTSExpr generics val
          pure $ TSBody [] (TSApp tsFunc tsVal)
        (MyConstructor _ tyCon) ->
          pure $ TSBody [] (TSData (prettyPrint tyCon) mempty)
        (MyIf _mtIf predExpr thenExpr elseExpr) -> do
          (TSBody as tsPred) <- makeTSExpr generics predExpr
          (TSBody bs tsThen) <- makeTSExpr generics thenExpr
          (TSBody cs tsElse) <- makeTSExpr generics elseExpr
          pure $ TSBody (as <> bs <> cs) (TSTernary tsPred tsThen tsElse)
        (MyRecord _ as) -> do
          tsExprs <- traverse (makeTSExpr generics) as
          let bodies = (\(TSBody a b) -> (a, b)) <$> tsExprs
              statements = mconcat (fst <$> M.elems bodies)
          pure $ TSBody statements (TSRecord (snd <$> bodies))
        (MyRecordAccess _ recExpr name) -> do
          (TSBody as tsExpr) <- makeTSExpr generics recExpr
          pure $ TSBody as (TSRecordAccess name tsExpr)
        (MyData _ dt rest) -> do
          let tsDataType = toTSDataType dt
          tell [tsDataType]
          makeTSExpr generics rest
        e -> error (show e)
