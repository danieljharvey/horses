{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.Codegen.ApplicativePure
  ( applicativePure,
  )
where

import Control.Applicative
import Control.Monad.Except
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Language.Mimsa.Typechecker.Codegen.Utils
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Prelude hiding (fmap)

applicativePure :: DataType -> Either Text (Expr Name ())
applicativePure = runCodegenM . applicativePure_

-- | `pure` takes the rightmost var and places it in the functor context
-- | ie A -> m A
-- | If there are multiple constructors that match this it will fail
applicativePure_ ::
  DataType ->
  CodegenM (Expr Name ())
applicativePure_ (DataType tyCon vars items) = do
  fVar <- getFunctorVar vars
  pureType <-
    singleVarConstructor fVar items
      <|> multiVarConstructor tyCon vars items
  expr' <- case pureType of
    PureVar tc ->
      pure $
        MyConsApp
          mempty
          (MyConstructor mempty tc)
          (MyVar mempty fVar)
    WithEmpties tc parts -> do
      foldl'
        ( \mExprA part -> do
            exprA <- mExprA
            partToExpr fVar items exprA part
        )
        (pure (MyConstructor mempty tc))
        parts

  pure
    ( MyLambda
        mempty
        fVar
        expr'
    )

partToExpr ::
  Name ->
  Map TyCon [Field] ->
  Expr Name () ->
  Part ->
  CodegenM (Expr Name ())
partToExpr fVar items innerExpr part =
  case part of
    VPart n ->
      if n == fVar
        then
          pure $
            MyConsApp
              mempty
              innerExpr
              (MyVar mempty n)
        else throwError "Cannot use non-functor value"
    TPart -> do
      emptyTyCon <- emptyConstructor items
      pure $
        MyConsApp
          mempty
          innerExpr
          (MyConstructor mempty emptyTyCon)
    FPart n a ->
      pure $ MyConsApp mempty innerExpr (MyLambda mempty n (MyVar mempty a))

data Part
  = VPart Name
  | TPart
  | FPart Name Name

data PureType
  = PureVar TyCon
  | WithEmpties TyCon (NonEmpty Part)

-- | a constructor with one variable, that is the functorVar one
singleVarConstructor :: Name -> Map TyCon [Field] -> CodegenM PureType
singleVarConstructor fVar items = do
  let filterFn (_tc, fields) = case fields of
        [VarName a] | a == fVar -> True
        _ -> False
  (tyCon, _) <- matchConstructor filterFn items
  pure (PureVar tyCon)

-- | an empty constructor
emptyConstructor :: Map TyCon [Field] -> CodegenM TyCon
emptyConstructor items = do
  let filterFn (_, fields) = null fields
  (k, _) <- matchConstructor filterFn items
  pure k

fieldIsRecursion :: TyCon -> [Name] -> Field -> Bool
fieldIsRecursion tyCon vars (ConsName tyCon' vars') =
  tyCon == tyCon' && and (zipWith fieldIsName vars vars')
fieldIsRecursion _ _ _ = False

fieldIsName :: Name -> Field -> Bool
fieldIsName name (VarName a) = name == a
fieldIsName _ _ = False

multiVarConstructor :: TyCon -> [Name] -> Map TyCon [Field] -> CodegenM PureType
multiVarConstructor tyCon vars items = do
  let withField (tc, fields) = case NE.nonEmpty fields of
        Nothing -> Nothing
        Just neFields -> do
          WithEmpties tc
            <$> traverse
              ( \case
                  VarName a -> Just (VPart a)
                  TNFunc (VarName a) (VarName b) ->
                    Just $ FPart a b
                  other ->
                    if fieldIsRecursion tyCon vars other
                      then Just TPart
                      else Nothing
              )
              neFields
  let matches = catMaybes (withField <$> M.toList items)
  case matches of
    [match] -> pure match
    _ -> throwError "No matches or too many"
