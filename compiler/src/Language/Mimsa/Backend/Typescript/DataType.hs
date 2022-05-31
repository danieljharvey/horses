{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Backend.Typescript.DataType (createConstructorFunctions) where

import Data.Coerce (coerce)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Backend.Typescript.Types
import Language.Mimsa.Printer
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Identifiers.TypeName

typeNameToTSName :: Int -> TSType -> TSName
typeNameToTSName _ (TSTypeVar a) = coerce (T.toLower a)
typeNameToTSName _ (TSType _ name _) = coerce (T.toLower name)
typeNameToTSName i _ = coerce $ "u" <> prettyPrint i

genericsForType :: TSType -> Set TSGeneric
genericsForType (TSTypeVar a) = S.singleton (TSGeneric a)
genericsForType (TSType _ _ as) = mconcat (genericsForType <$> as)
genericsForType (TSTypeFun _ f a) = genericsForType f <> genericsForType a
genericsForType (TSTypeArray a) = genericsForType a
genericsForType (TSTypeTuple as) = mconcat (genericsForType <$> as)
genericsForType (TSTypeRecord as) = mconcat (genericsForType <$> M.elems as)
genericsForType (TSTypeAnd a b) = genericsForType a <> genericsForType b

-- | Creates the return type of a constructor
returnType :: [Text] -> TypeName -> [TSType] -> TSType
returnType dtArgs typeName consArgs =
  TSType Nothing (coerce typeName) fixedConsArgs
  where
    allConsArgs = mconcat (genericsForType <$> consArgs)
    fixedConsArgs =
      ( \arg ->
          if S.member (TSGeneric arg) allConsArgs
            then TSTypeVar arg
            else TSType Nothing "never" mempty
      )
        <$> dtArgs

createConstructorFunctions :: TSDataType -> [TSStatement]
createConstructorFunctions (TSDataType typeName dtArgs constructors) =
  createConstructorFunction typeName dtArgs <$> constructors

-- | because we build constructors inside -> out, we can't look at the generics
-- we've used, so instead, we take the whole thing and remove them where
-- needed
removeRepeatedGenerics :: TSExpr -> TSExpr
removeRepeatedGenerics = removeSeen mempty
  where
    notUsedAlready alreadySeen =
      S.filter (\a -> not (S.member a alreadySeen))
    removeSeen seen (TSFunction a gen b c (TSFunctionBody (TSBody d rest))) =
      let newSeen = seen <> gen
       in TSFunction
            a
            (notUsedAlready seen gen)
            b
            c
            (TSFunctionBody (TSBody d (removeSeen newSeen rest)))
    removeSeen _ other = other

-- turn Just constructor into a function like  \a -> Just a
createConstructorFunction ::
  TypeName ->
  [Text] ->
  TSConstructor ->
  TSStatement
createConstructorFunction typeName dtArgs (TSConstructor tyCon []) =
  TSAssignment
    (TSVar (coerce tyCon))
    (Just (returnType dtArgs typeName mempty))
    (TSLetBody (TSBody [] (TSData (prettyPrint tyCon) mempty)))
createConstructorFunction typeName dtArgs (TSConstructor tyCon tsArgs) =
  let numberList = zip [1 ..] tsArgs
      args = (\(i, tn) -> TSVar (typeNameToTSName i tn)) <$> numberList
      tsData = TSData (prettyPrint tyCon) args
      foldFn (i, tsType) expr' =
        let variable = typeNameToTSName i tsType
            generics = genericsForType tsType
            isFinal = i == length numberList
            returnType' =
              if isFinal
                then Just (returnType dtArgs typeName tsArgs)
                else Nothing
         in TSFunction
              variable
              generics
              tsType
              returnType'
              (TSFunctionBody (TSBody mempty expr'))
      constructorFn =
        foldr foldFn tsData numberList
   in TSAssignment
        (TSVar (coerce tyCon))
        Nothing
        (TSLetBody (TSBody [] (removeRepeatedGenerics constructorFn)))
