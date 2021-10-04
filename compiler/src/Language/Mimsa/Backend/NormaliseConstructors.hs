{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Mimsa.Backend.NormaliseConstructors
  ( normaliseConstructors,
    getConsArgList,
    getNestedTyCons,
    containsConst,
  )
where

import Control.Monad.Except
import Data.Coerce
import Data.Foldable (foldl')
import qualified Data.Map as M
import Language.Mimsa.ExprUtils
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store.ResolvedTypeDeps
import Language.Mimsa.Types.Typechecker

-- turns Constructors into functions
normaliseConstructors ::
  (Monoid ann, MonadError (BackendError ann) m) =>
  ResolvedTypeDeps ->
  Expr Name ann ->
  m (Expr Name ann)
normaliseConstructors dt (MyConstructor _ tyCon) =
  pure $ constructorToFunctionWithApplication dt [] tyCon
normaliseConstructors dt (MyApp _ a val) = do
  restOfExpr <- bindExpr (normaliseConstructors dt) val
  if containsConst a
    then
      constructorToFunctionWithApplication
        dt
        (getConsArgList (MyApp mempty a restOfExpr))
        <$> getNestedTyCons a
    else pure (MyApp mempty a restOfExpr)
normaliseConstructors dt expr' =
  bindExpr (normaliseConstructors dt) expr'

containsConst :: Expr Name ann -> Bool
containsConst (MyConstructor _ _) = True
containsConst (MyApp _ f _) = containsConst f
containsConst _ = False

getNestedTyCons ::
  (MonadError (BackendError ann) m) =>
  Expr Name ann ->
  m TyCon
getNestedTyCons (MyApp _ a _) = getNestedTyCons a
getNestedTyCons (MyConstructor _ tyCon) = pure tyCon
getNestedTyCons (MyLambda _ _ a) = getNestedTyCons a
getNestedTyCons other = throwError (TyConFindError other)

getConsArgList :: Expr Name ann -> [Expr Name ann]
getConsArgList (MyApp _ (MyConstructor _ _tyCon) a) = [a]
getConsArgList (MyApp _ next a) = getConsArgList next <> [a]
getConsArgList a = [a]

typeNameToName :: Int -> Type ann -> Name
typeNameToName _ (MTVar _ (TVName name)) = coerce name
typeNameToName i _ = mkName $ "u" <> prettyPrint i

-- ffs
safeGetItem :: Int -> [a] -> Maybe a
safeGetItem i as =
  if length as <= i
    then Nothing
    else Just (as !! i)

-- turn Just constructor into a function like  \a -> Just a
constructorToFunctionWithApplication ::
  forall ann.
  (Monoid ann) =>
  ResolvedTypeDeps ->
  [Expr Name ann] ->
  TyCon ->
  Expr Name ann
constructorToFunctionWithApplication dt args tyCon =
  let tyVars =
        extractTypeConstructor tyCon
          <$> findDataTypeInProject dt tyCon
   in case tyVars of
        Just [] -> MyConstructor mempty tyCon
        Just as ->
          let numberList = zip [1 ..] as
              withConsApp =
                foldl'
                  ( \expr' (i, tn) ->
                      let variable = case safeGetItem (i - 1) args of
                            Just expression -> expression
                            Nothing -> MyVar mempty (typeNameToName i tn)
                       in MyApp mempty expr' variable
                  )
                  (MyConstructor mempty tyCon)
                  numberList
           in foldr
                ( \(i, tn) expr' ->
                    let variable = typeNameToName i tn
                     in MyLambda mempty variable expr'
                )
                withConsApp
                ( drop
                    (length args)
                    numberList
                )
        _ -> MyConstructor mempty tyCon

findDataTypeInProject :: ResolvedTypeDeps -> TyCon -> Maybe DataType
findDataTypeInProject (ResolvedTypeDeps dt) tyCon =
  snd <$> M.lookup tyCon dt

extractTypeConstructor ::
  TyCon ->
  DataType ->
  [Type ()]
extractTypeConstructor tc dt =
  case M.lookup tc (dtConstructors dt) of
    Just names -> names
    _ -> []
