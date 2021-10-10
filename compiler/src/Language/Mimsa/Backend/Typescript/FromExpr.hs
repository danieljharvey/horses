{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Backend.Typescript.FromExpr (fromExpr) where

import Control.Monad.Except
import Data.Coerce (coerce)
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import Data.Monoid (First (..), getFirst)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Language.Mimsa.Backend.Typescript.Monad
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

-- look for datatype containing constructor, return arguments if found
findDataTypeInProject :: [DataType] -> TyCon -> Maybe [Type ()]
findDataTypeInProject dts tyCon =
  getFirst $ mconcat (First . extractTypeConstructor tyCon <$> dts)

extractTypeConstructor ::
  TyCon ->
  DataType ->
  Maybe [Type ()]
extractTypeConstructor tc (DataType _ _ constructors) =
  let isMatch tyCon _ = tc == tyCon
   in case listToMaybe $ M.elems $ M.filterWithKey isMatch constructors of
        Just names -> Just names
        _ -> Nothing

typeNameToName :: Int -> Type ann -> Name
typeNameToName _ (MTVar _ (TVName name)) = coerce name
typeNameToName i _ = mkName $ "u" <> prettyPrint i

-- turn Just constructor into a function like  \a -> Just a
createConstructorFunction ::
  [DataType] ->
  TyCon ->
  TypescriptM TSBody
createConstructorFunction dt tyCon =
  let defaultEmpty =
        TSBody
          [ TSAssignment
              (TSPatternVar (coerce tyCon))
              (TSLetBody (TSBody [] (TSData (prettyPrint tyCon) mempty)))
          ]
          (TSVar (coerce tyCon))
   in case findDataTypeInProject dt tyCon of
        Just [] ->
          pure defaultEmpty
        Just as ->
          let numberList = zip [1 ..] as
              args = (\(i, tn) -> TSVar (typeNameToName i tn)) <$> numberList
              tsData = TSData (prettyPrint tyCon) args
              constructorFn =
                foldr
                  ( \(i, tn) expr' ->
                      let variable = typeNameToName i tn
                          (tsType, generics) = toTSType tn
                       in TSFunction variable generics tsType (TSFunctionBody (TSBody mempty expr'))
                  )
                  tsData
                  numberList
           in pure $
                TSBody
                  [ TSAssignment
                      (TSPatternVar (coerce tyCon))
                      (TSLetBody (TSBody [] constructorFn))
                  ]
                  (TSVar (coerce tyCon))
        _ -> throwError (ConstructorNotFound tyCon)

toInfix ::
  Operator ->
  Expr Name MonoType ->
  Expr Name MonoType ->
  TypescriptM TSExpr
toInfix operator a b = do
  tsA <- toTSExpr a
  tsB <- toTSExpr b
  case operator of
    Equals ->
      pure $ TSInfix TSEquals tsA tsB
    Add ->
      pure $ TSInfix TSAdd tsA tsB
    Subtract ->
      pure $ TSInfix TSSubtract tsA tsB
    StringConcat ->
      pure $ TSInfix TSStringConcat tsA tsB
    ArrayConcat ->
      pure $ TSInfix TSArrayConcat tsA tsB
    (Custom _op) -> error "custom infixes not implemented" -- we need to save these when they are defined and pull the correct function from state

-- | make TS body, but throw if we get any additional lines
-- a temporary measure so we can see how often these happen (because they don't
-- make sense often)
toTSExpr :: Expr Name MonoType -> TypescriptM TSExpr
toTSExpr expr' =
  toTSBody expr' >>= \case
    (TSBody [] expr) -> pure expr
    (TSBody as a) -> throwError (ExpectedExprGotBody a as)

fromExpr :: Expr Name MonoType -> Either (BackendError MonoType) TSModule
fromExpr expr = do
  (result, dataTypes) <- runTypescriptM (toTSBody expr)
  pure (TSModule dataTypes result)

toTSBody :: Expr Name MonoType -> TypescriptM TSBody
toTSBody expr' =
  case expr' of
    (MyLiteral _ lit) ->
      pure $ TSBody mempty (TSLit (toLiteral lit))
    (MyLet _ name letExpr letBody) -> do
      newLetExpr <- toTSBody letExpr
      let newBinding =
            TSAssignment
              (TSPatternVar name)
              (TSLetBody newLetExpr)
      (TSBody bindings' newExpr) <- toTSBody letBody
      pure (TSBody ([newBinding] <> bindings') newExpr)
    (MyLetPattern _ pat letExpr letBody) -> do
      newLetExpr <- toTSBody letExpr
      let newBinding =
            TSAssignment
              (toPattern pat)
              (TSLetBody newLetExpr)
      (TSBody bindings' newExpr) <- toTSBody letBody
      pure (TSBody ([newBinding] <> bindings') newExpr)
    (MyPair _ a b) -> do
      tsA <- toTSExpr a
      tsB <- toTSExpr b
      pure (TSBody mempty (TSArray [tsA, tsB]))
    (MyVar _ a) -> pure (TSBody mempty (TSVar a))
    (MyLambda fnType bind body) -> do
      let (mtFn, generics') = toTSType fnType
          mtArg = case mtFn of
            (TSTypeFun _ a _) -> a
            _ -> error "function does not have function type"
      -- get diff between generics we've not used yet
      newGenerics <- unusedGenerics generics'
      -- continue....
      tsBody <- toTSBody body
      pure $
        TSBody
          []
          ( TSFunction
              bind
              newGenerics
              mtArg
              ( TSFunctionBody tsBody
              )
          )
    (MyPatternMatch _mtPatternMatch matchExpr patterns) -> do
      matches <-
        traverse
          ( \(pat, patExpr) -> do
              let tsPat = toPattern pat
              (TSBody parts tsPatExpr) <- toTSBody patExpr
              let item = TSAssignment tsPat (TSLetBody (TSBody [] (TSVar "value")))
              pure $
                TSConditional
                  (toPattern pat)
                  (TSLetBody (TSBody (item : parts) tsPatExpr))
          )
          patterns
      tsA <- toTSExpr matchExpr
      let (tyMatchExpr, matchGenerics) = toTSType (getAnnotation matchExpr)
      newGenerics <- unusedGenerics matchGenerics
      pure $
        TSBody
          [ TSAssignment
              (TSPatternVar "match")
              ( TSLetBody
                  ( TSBody
                      []
                      ( TSFunction
                          "value"
                          newGenerics
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
    (MyApp _mtApp func val) -> do
      (TSBody as tsFunc) <- toTSBody func
      (TSBody bs tsVal) <- toTSBody val
      pure $ TSBody (as <> bs) (TSApp tsFunc tsVal)
    (MyConstructor _ tyCon) -> do
      -- this case should only happen when finding a lone constructor
      -- which we should then transform into a function
      state <- getState
      createConstructorFunction (csDataTypes state) tyCon
    (MyIf _mtIf predExpr thenExpr elseExpr) -> do
      (TSBody as tsPred) <- toTSBody predExpr
      (TSBody bs tsThen) <- toTSBody thenExpr
      (TSBody cs tsElse) <- toTSBody elseExpr
      pure $ TSBody (as <> bs <> cs) (TSTernary tsPred tsThen tsElse)
    (MyRecord _ as) -> do
      tsExprs <- traverse toTSBody as
      let bodies = (\(TSBody a b) -> (a, b)) <$> tsExprs
          statements = mconcat (fst <$> M.elems bodies)
      pure $ TSBody statements (TSRecord (snd <$> bodies))
    (MyRecordAccess _ recExpr name) -> do
      (TSBody as tsExpr) <- toTSBody recExpr
      pure $ TSBody as (TSRecordAccess name tsExpr)
    (MyData _ dt rest) -> do
      let tsDataType = toTSDataType dt
      addDataType dt tsDataType
      toTSBody rest
    (MyInfix _ op a b) -> do
      TSBody [] <$> toInfix op a b
    e -> error (show e)
