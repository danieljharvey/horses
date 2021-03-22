{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Backend.NormaliseConstructors
  ( normaliseConstructors,
    getConsArgList,
    getNestedTyCons,
  )
where

import Control.Monad.Except
import Data.Coerce
import Data.Foldable (foldl')
import qualified Data.Map as M
import Language.Mimsa.Backend.Types
import Language.Mimsa.ExprUtils
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store.ResolvedTypeDeps
import Language.Mimsa.Types.Typechecker

-- turns Constructors into functions
normaliseConstructors ::
  (Monoid ann) =>
  (ResolvedTypeDeps ann) ->
  Expr Name ann ->
  BackendM ann (Expr Name ann)
normaliseConstructors dt (MyConstructor _ tyCon) =
  pure $ constructorToFunctionWithApplication dt [] tyCon
normaliseConstructors dt (MyConsApp _ a val) = do
  restOfExpr <- bindExpr (normaliseConstructors dt) val
  constructorToFunctionWithApplication
    dt
    (getConsArgList (MyConsApp mempty a restOfExpr))
    <$> getNestedTyCons a
normaliseConstructors dt expr' =
  bindExpr (normaliseConstructors dt) expr'

getNestedTyCons :: Expr Name ann -> BackendM ann TyCon
getNestedTyCons (MyConsApp _ a _) = getNestedTyCons a
getNestedTyCons (MyConstructor _ tyCon) = pure tyCon
getNestedTyCons (MyLambda _ _ a) = getNestedTyCons a
getNestedTyCons other = throwError (TyConFindError other)

getConsArgList :: Expr Name ann -> [Expr Name ann]
getConsArgList (MyConsApp _ (MyConstructor _ _tyCon) a) = [a]
getConsArgList (MyConsApp _ next a) = getConsArgList next <> [a]
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
  (Monoid ann) =>
  (ResolvedTypeDeps ann) ->
  [Expr Name ann] ->
  TyCon ->
  Expr Name ann
constructorToFunctionWithApplication dt args tyCon =
  let tyVars = extractTypeConstructor tyCon <$> findDataTypeInProject dt tyCon
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
                       in MyConsApp mempty expr' variable
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

findDataTypeInProject :: (ResolvedTypeDeps ann) -> TyCon -> Maybe (DataType ann)
findDataTypeInProject (ResolvedTypeDeps dt) tyCon =
  snd <$> M.lookup tyCon dt

extractTypeConstructor :: TyCon -> (DataType ann) -> [Type ann]
extractTypeConstructor tc dt =
  case M.lookup tc (dtConstructors dt) of
    Just names -> names
    _ -> []
