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

tsTypeNameToName :: Int -> TSType -> TSName
tsTypeNameToName _ (TSTypeVar a) = coerce (T.toLower a)
tsTypeNameToName _ (TSType _ name _) = coerce (T.toLower name)
tsTypeNameToName i _ = coerce $ "u" <> prettyPrint i

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
      args = (\(i, tn) -> TSVar (tsTypeNameToName i tn)) <$> numberList
      tsData = TSData (prettyPrint tyCon) args
      foldFn (i, tsType) expr' =
        let variable = tsTypeNameToName i tsType
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
        (TSLetBody (TSBody [] constructorFn))
