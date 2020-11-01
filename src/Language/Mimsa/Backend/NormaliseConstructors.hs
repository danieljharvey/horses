{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Backend.NormaliseConstructors where

import Data.Foldable (foldl')
import qualified Data.Map as M
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.ResolvedTypeDeps

-- turns Constructors into functions
normaliseConstructors ::
  (Monoid ann) =>
  ResolvedTypeDeps ->
  Expr Name ann ->
  Expr Name ann
normaliseConstructors dataTypes (MyConstructor _ tyCon) =
  constructorToFunctionWithApplication dataTypes [] tyCon
normaliseConstructors dataTypes (MyConsApp _ a b) =
  constructorToFunctionWithApplication
    dataTypes
    (getConsArgList (MyConsApp mempty a b))
    (getNestedTyCons a)
normaliseConstructors _ a = a

getNestedTyCons :: Expr Name ann -> TyCon
getNestedTyCons (MyConsApp _ a _) = getNestedTyCons a
getNestedTyCons (MyConstructor _ tyCon) = tyCon
getNestedTyCons _ = error "This is bad news" -- forgive me padre

getConsArgList :: Expr Name ann -> [Expr Name ann]
getConsArgList (MyConsApp _ (MyConstructor _ _tyCon) a) = [a]
getConsArgList (MyConsApp _ next a) = [a] <> getConsArgList next
getConsArgList a = [a]

typeNameToName :: Int -> TypeName -> Name
typeNameToName _ (VarName name) = name
typeNameToName i _ = mkName $ "U" <> prettyPrint i

-- ffs
safeGetItem :: Int -> [a] -> Maybe a
safeGetItem i as =
  if length as <= i
    then Nothing
    else Just (as !! i)

-- turn Just constructor into a function like  \a -> Just a
constructorToFunctionWithApplication ::
  (Monoid ann) =>
  ResolvedTypeDeps ->
  [Expr Name ann] ->
  TyCon ->
  Expr Name ann
constructorToFunctionWithApplication dataTypes args tyCon =
  let tyVars = extractTypeConstructor tyCon <$> findDataTypeInProject dataTypes tyCon
   in case tyVars of
        Just [] -> MyConstructor mempty tyCon
        Just as ->
          let numberList = zip [1 ..] as
              withConsApp =
                foldl'
                  ( \expr' (i, tn) ->
                      let variable = case safeGetItem (i - 1) (reverse args) of
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

findDataTypeInProject :: ResolvedTypeDeps -> TyCon -> Maybe DataType
findDataTypeInProject (ResolvedTypeDeps dataTypes) tyCon =
  snd <$> M.lookup tyCon dataTypes

extractTypeConstructor :: TyCon -> DataType -> [TypeName]
extractTypeConstructor tc dt =
  case M.lookup tc (dtConstructors dt) of
    Just names -> names
    _ -> []
