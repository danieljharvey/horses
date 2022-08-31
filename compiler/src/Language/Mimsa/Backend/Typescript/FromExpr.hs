{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Backend.Typescript.FromExpr (fromExpr) where

import Control.Monad.Except
import Data.Bifunctor
import Data.Coerce (coerce)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
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

bimapMap :: (Ord j) => (k -> j) -> (a -> b) -> Map k a -> Map j b
bimapMap f g = M.fromList . fmap (bimap f g) . M.toList

toLiteral :: Literal -> TSLiteral
toLiteral lit = case lit of
  (MyInt i) -> TSInt i
  (MyBool b) -> TSBool b
  (MyString (StringType s)) -> TSString s

toArraySpread :: Spread Name ann -> TSSpread
toArraySpread (SpreadValue _ a) = TSSpreadValue (coerce a)
toArraySpread (SpreadWildcard _) = TSSpreadWildcard
toArraySpread NoSpread = TSNoSpread

toStringPart :: StringPart Name ann -> TSStringPart
toStringPart (StrValue _ a) = TSStringVar (coerce a)
toStringPart (StrWildcard _) = TSStringWildcard

toPattern :: Pattern Name ann -> TSPattern
toPattern (PVar _ a) =
  TSPatternVar (coerce a)
toPattern (PPair _ a b) =
  TSPatternPair (toPattern a) (toPattern b)
toPattern (PWildcard _) =
  TSPatternWildcard
toPattern (PConstructor _ _ name vars) =
  TSPatternConstructor name (toPattern <$> vars)
toPattern (PRecord _ pMap) =
  TSPatternRecord (bimapMap coerce toPattern pMap)
toPattern (PArray _ as spread) =
  TSPatternArray (toPattern <$> as) (toArraySpread spread)
toPattern (PLit _ lit) =
  TSPatternLit (toLiteral lit)
toPattern (PString _ sHead sTail) =
  TSPatternString (toStringPart sHead) (toStringPart sTail)

consToTSType :: Type ann -> TypescriptM (TSType, Set TSGeneric)
consToTSType mt =
  case varsFromDataType mt of
    Just (_modName, TypeName n, vars) -> do
      imported <- typeNameIsImport (TypeName n)
      let namespace =
            if imported
              then Just n
              else Nothing
      tsTypes <- traverse toTSType vars
      let (types, generics) = unzip tsTypes
      pure (TSType namespace n types, mconcat generics)
    Nothing ->
      throwError NoConstructorInTypeApp

toTSTypeRecord :: Map Name (Type ann) -> TypescriptM (TSType, Set TSGeneric)
toTSTypeRecord as = do
  tsAll <- traverse toTSType as
  let generics = snd . snd <$> M.toList tsAll
      tsItems = M.fromList . fmap (bimap coerce fst) . M.toList $ tsAll
  pure (TSTypeRecord tsItems, mconcat generics)

toTSType :: Type ann -> TypescriptM (TSType, Set TSGeneric)
toTSType = toTSType' False

-- | returns the type and any generics used in the expression
toTSType' :: Bool -> Type ann -> TypescriptM (TSType, Set TSGeneric)
toTSType' _ (MTPrim _ MTString) = pure (TSType Nothing "string" [], mempty)
toTSType' _ (MTPrim _ MTInt) = pure (TSType Nothing "number" [], mempty)
toTSType' _ (MTPrim _ MTBool) = pure (TSType Nothing "boolean" [], mempty)
toTSType' _ (MTVar _ a) =
  let newVar = case a of
        TVUnificationVar i' -> T.toTitle (T.pack (printTypeNum (i' + 1)))
        TVName a' -> T.toTitle (coerce a')
        TVScopedVar i' _ -> T.toTitle (T.pack (printTypeNum (i' + 1)))
   in pure (TSTypeVar newVar, S.singleton (TSGeneric newVar))
toTSType' _ mt@MTTypeApp {} =
  consToTSType mt
toTSType' topLevel (MTFunction _ a b) = do
  (tsA, genA) <- toTSType' False a
  (tsB, genB) <- toTSType' False b
  let generics =
        if topLevel
          then genA -- we don't want to include generics from later args in curried functions
          else genA <> genB -- but we do want later args of higher-order functions
  pure (TSTypeFun "arg" tsA tsB, generics)
toTSType' _ (MTArray _ as) = do
  (tsAs, genAs) <- toTSType as
  pure (TSTypeArray tsAs, genAs)
toTSType' _ (MTPair _ a b) = do
  (tsA, genA) <- toTSType a
  (tsB, genB) <- toTSType b
  pure (TSTypeTuple [tsA, tsB], genA <> genB)
toTSType' _ mt@MTConstructor {} =
  consToTSType mt
toTSType' _ (MTRecord _ as) =
  toTSTypeRecord as
toTSType' _ (MTRecordRow _ as rest) = do
  (tsItems, generics) <- toTSTypeRecord as
  (tsRest, genRest) <- toTSType rest
  pure (TSTypeAnd tsItems tsRest, generics <> genRest)

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
    Equals -> do
      addImport
        (TSImportValue "equals_")
      pure $
        TSApp (TSApp (TSVar "equals_") tsA) tsB
    Add ->
      pure $ TSInfix TSAdd tsA tsB
    Subtract ->
      pure $ TSInfix TSSubtract tsA tsB
    GreaterThan ->
      pure $ TSInfix TSGreaterThan tsA tsB
    GreaterThanOrEqualTo ->
      pure $ TSInfix TSGreaterThanOrEqualTo tsA tsB
    LessThan ->
      pure $ TSInfix TSLessThan tsA tsB
    LessThanOrEqualTo ->
      pure $ TSInfix TSLessThanOrEqualTo tsA tsB
    StringConcat ->
      pure $ TSInfix TSStringConcat tsA tsB
    ArrayConcat ->
      pure $ TSArray [TSArraySpread tsA, TSArraySpread tsB]
    (Custom op) -> do
      expr <- findInfix op
      pure (TSApp (TSApp expr tsA) tsB)

-- | make TS body, but throw if we get any additional lines
-- a temporary measure so we can see how often these happen (because they don't
-- make sense often)
toTSExpr :: Expr Name MonoType -> TypescriptM TSExpr
toTSExpr expr' =
  toTSBody expr' >>= \case
    (TSBody [] expr) -> pure expr
    (TSBody as a) -> throwError (ExpectedExprGotBody a as)

fromExpr ::
  TSReaderState ->
  TSCodegenState ->
  Expr Name MonoType ->
  Either (BackendError MonoType) (TSModule, [TSImport])
fromExpr readerState startState expr = do
  (result, dataTypes, imports) <- runTypescriptM readerState startState (toTSBody expr)
  pure (TSModule dataTypes result, imports)

identifierName :: Identifier Name ann -> Name
identifierName ident = case ident of
  Identifier _ n -> n

toLet :: Identifier Name MonoType -> Expr Name MonoType -> Expr Name MonoType -> TypescriptM TSBody
toLet ident letExpr letBody = do
  newLetExpr <- toTSBody letExpr
  let newBinding =
        TSAssignment
          (TSVar (coerce (identifierName ident)))
          Nothing
          (TSLetBody newLetExpr)
  (TSBody bindings' newExpr) <- toTSBody letBody
  pure (TSBody ([newBinding] <> bindings') newExpr)

toLetPattern ::
  Pattern Name MonoType ->
  Expr Name MonoType ->
  Expr Name MonoType ->
  TypescriptM TSBody
toLetPattern pat letExpr letBody = do
  newLetExpr <- toTSBody letExpr
  let (tsPatExpr, statements) = getDestructureExpr TSUnderscore (toPattern pat)
  let newBinding =
        TSAssignment
          tsPatExpr
          Nothing
          (TSLetBody newLetExpr)
  (TSBody bindings' newExpr) <- toTSBody letBody
  pure (TSBody ([newBinding] <> bindings' <> statements) newExpr)

toLambda ::
  MonoType ->
  Identifier Name MonoType ->
  Expr Name MonoType ->
  TypescriptM TSBody
toLambda fnType ident body = do
  (mtFn, generics') <- toTSType' True fnType
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
          (coerce (identifierName ident))
          newGenerics
          mtArg
          Nothing
          ( TSFunctionBody tsBody
          )
      )

toPatternStatement ::
  (Pattern Name MonoType, Expr Name MonoType) ->
  TypescriptM TSStatement
toPatternStatement (pat, patExpr) = do
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
      (TSLetBody (TSBody (statements <> items) tsPatExpr))

getMatchReturnType :: [(a, Expr Name MonoType)] -> TypescriptM TSType
getMatchReturnType as = case as of
  ((_pat, expr) : _) -> fst <$> toTSType (getAnnotation expr)
  _ -> throwError PatternMatchIsEmpty

toPatternMatch ::
  Expr Name MonoType ->
  [(Pattern Name MonoType, Expr Name MonoType)] ->
  TypescriptM TSBody
toPatternMatch matchExpr patterns = do
  matches <- traverse toPatternStatement patterns
  (TSBody tsStatements tsA) <- toTSBody matchExpr
  (tyMatchExpr, matchGenerics) <- toTSType (getAnnotation matchExpr)
  newGenerics <- unusedGenerics matchGenerics
  returnType <- getMatchReturnType patterns
  let assignment =
        TSAssignment
          (TSVar "match")
          Nothing
          ( TSLetBody
              ( TSBody
                  []
                  ( TSFunction
                      "value"
                      newGenerics
                      tyMatchExpr
                      (Just returnType)
                      ( TSFunctionBody
                          ( TSBody
                              matches
                              (TSError "Pattern match error")
                          )
                      )
                  )
              )
          )

  pure $
    TSBody
      (tsStatements <> [assignment])
      (TSApp (TSVar "match") tsA)

toTSBody :: Expr Name MonoType -> TypescriptM TSBody
toTSBody expr' =
  case expr' of
    (MyLiteral _ lit) ->
      pure $ TSBody mempty (TSLit (toLiteral lit))
    (MyAnnotation _ _ expr) -> toTSBody expr
    (MyLet _ ident letExpr letBody) ->
      toLet ident letExpr letBody
    (MyLetPattern _ pat letExpr letBody) ->
      toLetPattern pat letExpr letBody
    (MyPair _ a b) -> do
      tsA <- toTSExpr a
      tsB <- toTSExpr b
      pure (TSBody mempty (TSPair tsA tsB))
    (MyVar _ _ a) ->
      pure (TSBody mempty (TSVar (coerce a)))
    (MyLambda fnType bind body) ->
      toLambda fnType bind body
    (MyPatternMatch _mtPatternMatch matchExpr patterns) ->
      toPatternMatch matchExpr patterns
    (MyApp _mtApp func val) -> do
      (TSBody as tsFunc) <- toTSBody func
      (TSBody bs tsVal) <- toTSBody val
      pure $ TSBody (as <> bs) (TSApp tsFunc tsVal)
    (MyConstructor _ _ tyCon) -> do
      namespace <- findTypeName tyCon
      pure $
        TSBody [] $ case namespace of
          Just typeName ->
            TSRecordAccess (coerce tyCon) (TSVar (coerce typeName))
          _ -> TSVar (coerce tyCon)
    (MyIf _mtIf predExpr thenExpr elseExpr) -> do
      (TSBody as tsPred) <- toTSBody predExpr
      (TSBody bs tsThen) <- toTSBody thenExpr
      (TSBody cs tsElse) <- toTSBody elseExpr
      pure $ TSBody (as <> bs <> cs) (TSTernary tsPred tsThen tsElse)
    (MyRecord _ as) -> do
      tsExprs <- traverse toTSBody as
      let bodies = (\(TSBody a b) -> (a, b)) <$> tsExprs
          statements = mconcat (fst <$> M.elems bodies)
      pure $ TSBody statements (TSRecord (bimapMap coerce snd bodies))
    (MyRecordAccess _ recExpr name) -> do
      (TSBody as tsExpr) <- toTSBody recExpr
      pure $ TSBody as (TSRecordAccess (coerce name) tsExpr)
    (MyData _ dt rest) -> do
      tsDt <- toTSDataType dt
      addDataType dt tsDt
      toTSBody rest
    (MyInfix _ op a b) -> do
      TSBody [] <$> toInfix op a b
    (MyArray _ as) -> do
      tsAs <- (fmap . fmap) TSArrayItem (traverse toTSExpr as)
      pure $ TSBody [] (TSArray tsAs)
    (MyTypedHole _ name) ->
      throwError (OutputingTypedHole name)
    (MyDefineInfix _ op fn body) -> do
      fnExpr <- toTSExpr fn
      addInfix op fnExpr
      toTSBody body
