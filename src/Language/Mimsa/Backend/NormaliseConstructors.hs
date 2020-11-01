{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Backend.NormaliseConstructors
  ( normaliseConstructors,
    getConsArgList,
    getNestedTyCons,
  )
where

import Data.Bifunctor
import Data.Foldable (foldl')
import qualified Data.Map as M
import Language.Mimsa.Logging
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
normaliseConstructors dt (MyConstructor _ tyCon) =
  constructorToFunctionWithApplication dt [] tyCon
normaliseConstructors dt (MyConsApp _ a val) =
  let normalVal = normaliseConstructors dt val
   in constructorToFunctionWithApplication
        dt
        (getConsArgList (MyConsApp mempty a normalVal))
        (getNestedTyCons a)
normaliseConstructors _ (MyLiteral a b) = MyLiteral a b
normaliseConstructors _ (MyVar a b) = MyVar a b
normaliseConstructors dt (MyLet ann binder bindExpr inExpr) =
  MyLet
    ann
    binder
    (normaliseConstructors dt bindExpr)
    (normaliseConstructors dt inExpr)
normaliseConstructors dt (MyLetPair ann binderA binderB bindExpr inExpr) =
  MyLetPair
    ann
    binderA
    binderB
    (normaliseConstructors dt bindExpr)
    (normaliseConstructors dt inExpr)
normaliseConstructors dt (MyInfix ann op a b) =
  MyInfix ann op (normaliseConstructors dt a) (normaliseConstructors dt b)
normaliseConstructors dt (MyLambda ann binder expr) =
  MyLambda ann binder (normaliseConstructors dt expr)
normaliseConstructors dt (MyApp ann f a) =
  MyApp ann (normaliseConstructors dt f) (normaliseConstructors dt a)
normaliseConstructors dt (MyIf ann predExpr thenExpr elseExpr) =
  MyIf
    ann
    (normaliseConstructors dt predExpr)
    (normaliseConstructors dt thenExpr)
    (normaliseConstructors dt elseExpr)
normaliseConstructors dt (MyPair ann a b) =
  MyPair
    ann
    (normaliseConstructors dt a)
    (normaliseConstructors dt b)
normaliseConstructors dt (MyRecord ann items) =
  MyRecord ann (normaliseConstructors dt <$> items)
normaliseConstructors dt (MyRecordAccess ann recordExpr name) =
  MyRecordAccess ann (normaliseConstructors dt recordExpr) name
normaliseConstructors dt (MyData ann dt' a) =
  MyData ann dt' (normaliseConstructors dt a)
normaliseConstructors dt (MyCaseMatch ann matchExpr matches catchAll) =
  MyCaseMatch
    ann
    (normaliseConstructors dt matchExpr)
    ((fmap . second) (normaliseConstructors dt) matches)
    (normaliseConstructors dt <$> catchAll)

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
constructorToFunctionWithApplication dt args tyCon =
  let tyVars = extractTypeConstructor tyCon <$> findDataTypeInProject dt tyCon
   in debugPretty "cons normal" $ case tyVars of
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
findDataTypeInProject (ResolvedTypeDeps dt) tyCon =
  snd <$> M.lookup tyCon dt

extractTypeConstructor :: TyCon -> DataType -> [TypeName]
extractTypeConstructor tc dt =
  case M.lookup tc (dtConstructors dt) of
    Just names -> names
    _ -> []
