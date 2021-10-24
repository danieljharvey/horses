{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Backend.Typescript.FromExpr (fromExpr) where

import Control.Monad.Except
import Data.Coerce (coerce)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Language.Mimsa.Backend.Typescript.Monad
import Language.Mimsa.Backend.Typescript.Patterns
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

toArraySpread :: Spread Name ann -> TSSpread
toArraySpread (SpreadValue _ a) = TSSpreadValue a
toArraySpread (SpreadWildcard _) = TSSpreadWildcard
toArraySpread NoSpread = TSNoSpread

toStringPart :: StringPart Name ann -> TSStringPart
toStringPart (StrValue _ a) = TSStringVar a
toStringPart (StrWildcard _) = TSStringWildcard

toPattern :: Pattern Name ann -> TSPattern
toPattern (PVar _ a) =
  TSPatternVar a
toPattern (PPair _ a b) =
  TSPatternPair (toPattern a) (toPattern b)
toPattern (PWildcard _) =
  TSPatternWildcard
toPattern (PConstructor _ name vars) =
  TSPatternConstructor name (toPattern <$> vars)
toPattern (PRecord _ pMap) =
  TSPatternRecord (toPattern <$> pMap)
toPattern (PArray _ as spread) =
  TSPatternArray (toPattern <$> as) (toArraySpread spread)
toPattern (PLit _ lit) =
  TSPatternLit (toLiteral lit)
toPattern (PString _ sHead sTail) =
  TSPatternString (toStringPart sHead) (toStringPart sTail)

-- | returns the type and any generics used in the expression
toTSType :: Type ann -> TypescriptM (TSType, Set TSGeneric)
toTSType (MTPrim _ MTString) = pure (TSType Nothing "string" [], mempty)
toTSType (MTPrim _ MTInt) = pure (TSType Nothing "number" [], mempty)
toTSType (MTPrim _ MTBool) = pure (TSType Nothing "boolean" [], mempty)
toTSType (MTVar _ a) =
  let newVar = case a of
        TVNum i' -> T.toTitle (T.pack (printTypeNum (i' + 1)))
        TVName a' -> T.toTitle (coerce a')
   in pure (TSTypeVar newVar, S.singleton (TSGeneric newVar))
toTSType mt@MTTypeApp {} =
  case varsFromDataType mt of
    Just (TyCon n, vars) -> do
      imported <- typeNameIsImport (TyCon n)
      let namespace = if imported then Just n else Nothing
      tsTypes <- traverse toTSType vars
      let (types, generics) = unzip tsTypes
      pure (TSType namespace n types, mconcat generics)
    Nothing ->
      pure (TSType Nothing "weird type app error" mempty, mempty)
toTSType (MTFunction _ a b) = do
  (tsA, genA) <- toTSType a
  (tsB, genB) <- toTSType b
  pure (TSTypeFun "arg" tsA tsB, genA <> genB)
toTSType (MTArray _ as) = do
  (tsAs, genAs) <- toTSType as
  pure (TSTypeArray tsAs, genAs)
toTSType e =
  pure (TSType Nothing ("unknown for: " <> prettyPrint e) mempty, mempty)

toTSDataType :: DataType -> TypescriptM TSDataType
toTSDataType (DataType name gens cons) = do
  let toTSCons (tyCon, con) = do
        tsTypes' <- traverse toTSType con
        pure $ TSConstructor tyCon (fst <$> tsTypes')
  tsTypes <- traverse toTSCons (M.toList cons)
  pure $
    TSDataType
      name
      (T.toTitle . prettyPrint <$> gens)
      tsTypes

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
      pure $ TSArray [TSArraySpread tsA, TSArraySpread tsB]
    (Custom _op) -> error "custom infixes not implemented" -- we need to save these when they are defined and pull the correct function from state

-- | make TS body, but throw if we get any additional lines
-- a temporary measure so we can see how often these happen (because they don't
-- make sense often)
toTSExpr :: Expr Name MonoType -> TypescriptM TSExpr
toTSExpr expr' =
  toTSBody expr' >>= \case
    (TSBody [] expr) -> pure expr
    (TSBody as a) -> throwError (ExpectedExprGotBody a as)

fromExpr :: TSReaderState -> Expr Name MonoType -> Either (BackendError MonoType) TSModule
fromExpr readerState expr = do
  (result, dataTypes) <- runTypescriptM readerState (toTSBody expr)
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
              (TSVar name)
              Nothing
              (TSLetBody newLetExpr)
      (TSBody bindings' newExpr) <- toTSBody letBody
      pure (TSBody ([newBinding] <> bindings') newExpr)
    (MyLetPattern _ pat letExpr letBody) -> do
      newLetExpr <- toTSBody letExpr
      let (tsPatExpr, statements) = getDestructureExpr TSUnderscore (toPattern pat)
      let newBinding =
            TSAssignment
              tsPatExpr
              Nothing
              (TSLetBody newLetExpr)
      (TSBody bindings' newExpr) <- toTSBody letBody
      pure (TSBody ([newBinding] <> bindings' <> statements) newExpr)
    (MyPair _ a b) -> do
      tsA <- toTSExpr a
      tsB <- toTSExpr b
      pure (TSBody mempty (TSArray [TSArrayItem tsA, TSArrayItem tsB]))
    (MyVar _ a) -> pure (TSBody mempty (TSVar a))
    (MyLambda fnType bind body) -> do
      (mtFn, generics') <- toTSType fnType
      mtArg <- case mtFn of
        (TSTypeFun _ a _) -> pure a
        e -> throwError (ExpectedFunctionType e)
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
              Nothing
              ( TSFunctionBody tsBody
              )
          )
    (MyPatternMatch _mtPatternMatch matchExpr patterns) -> do
      matches <-
        traverse
          ( \(pat, patExpr) -> do
              let (tsExpr, statements) =
                    getDestructureExpr (TSVar "value") (toPattern pat)
              (TSBody parts tsPatExpr) <- toTSBody patExpr
              let items =
                    if tsExpr /= TSUnderscore
                      then
                        TSAssignment
                          tsExpr
                          Nothing
                          ( TSLetBody
                              ( TSBody [] (TSVar "value")
                              )
                          ) :
                        parts
                      else parts
              pure $
                TSConditional
                  (conditions $ toPattern pat)
                  (TSLetBody (TSBody (items <> statements) tsPatExpr))
          )
          patterns
      (TSBody tsStatements tsA) <- toTSBody matchExpr
      (tyMatchExpr, matchGenerics) <- toTSType (getAnnotation matchExpr)
      newGenerics <- unusedGenerics matchGenerics
      pure $
        TSBody
          ( tsStatements
              <> [ TSAssignment
                     (TSVar "match")
                     Nothing
                     ( TSLetBody
                         ( TSBody
                             []
                             ( TSFunction
                                 "value"
                                 newGenerics
                                 tyMatchExpr
                                 Nothing
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
          )
          (TSApp (TSVar "match") tsA)
    (MyApp _mtApp func val) -> do
      (TSBody as tsFunc) <- toTSBody func
      (TSBody bs tsVal) <- toTSBody val
      pure $ TSBody (as <> bs) (TSApp tsFunc tsVal)
    (MyConstructor _ tyCon) -> do
      namespace <- findTypeName tyCon
      let expr = case namespace of
            Just typeName ->
              TSRecordAccess (coerce tyCon) (TSVar (coerce typeName))
            _ -> TSVar (coerce tyCon)
      pure $ TSBody [] expr
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
      tsDt <- toTSDataType dt
      addDataType dt tsDt
      toTSBody rest
    (MyInfix _ op a b) -> do
      TSBody [] <$> toInfix op a b
    (MyArray _ as) -> do
      tsAs <- (fmap . fmap) TSArrayItem (traverse toTSExpr as)
      pure $ TSBody [] (TSArray tsAs)
    e -> error (show e)
