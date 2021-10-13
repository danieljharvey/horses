{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Backend.Typescript.DataType (createConstructorFunctions) where

import Data.Coerce (coerce)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Backend.Typescript.Types
import Language.Mimsa.Printer
import Language.Mimsa.Types.Identifiers

typeNameToName :: Int -> TSType -> Name
typeNameToName _ (TSTypeVar a) = coerce (T.toLower a)
typeNameToName _ (TSType name _) = coerce (T.toLower name)
typeNameToName i _ = coerce $ "u" <> prettyPrint i

genericsForType :: TSType -> Set TSGeneric
genericsForType (TSTypeVar a) = S.singleton (TSGeneric a)
genericsForType (TSType _ as) = mconcat (genericsForType <$> as)
genericsForType (TSTypeFun _ f a) = genericsForType f <> genericsForType a

-- | Creates the return type of a constructor
returnType :: [Text] -> TyCon -> [TSType] -> TSType
returnType dtArgs typeName consArgs =
  TSType (coerce typeName) fixedConsArgs
  where
    allConsArgs = mconcat (genericsForType <$> consArgs)
    fixedConsArgs =
      ( \arg ->
          if S.member (TSGeneric arg) allConsArgs
            then TSTypeVar arg
            else TSType "never" mempty
      )
        <$> dtArgs

createConstructorFunctions :: TSDataType -> [TSStatement]
createConstructorFunctions (TSDataType typeName dtArgs constructors) =
  createConstructorFunction typeName dtArgs <$> constructors

-- turn Just constructor into a function like  \a -> Just a
createConstructorFunction ::
  TyCon ->
  [Text] ->
  TSConstructor ->
  TSStatement
createConstructorFunction typeName dtArgs (TSConstructor tyCon tsArgs) =
  -- do
  --  tsArgs <- extractTypeConstructor tyCon dt
  case tsArgs of
    [] ->
      TSAssignment
        (TSPatternVar (coerce tyCon))
        (Just (returnType dtArgs typeName mempty))
        (TSLetBody (TSBody [] (TSData (prettyPrint tyCon) mempty)))
    as ->
      let numberList = zip [1 ..] as
          args = (\(i, tn) -> TSVar (typeNameToName i tn)) <$> numberList
          tsData = TSData (prettyPrint tyCon) args
          constructorFn =
            foldr
              ( \(i, tsType) expr' ->
                  let variable = typeNameToName i tsType
                      generics = genericsForType tsType
                      isFinal = i == length numberList
                      returnType' =
                        if isFinal
                          then Just (returnType dtArgs typeName tsArgs)
                          else Nothing
                   in TSFunction variable generics tsType returnType' (TSFunctionBody (TSBody mempty expr'))
              )
              tsData
              numberList
       in TSAssignment
            (TSPatternVar (coerce tyCon))
            Nothing
            (TSLetBody (TSBody [] constructorFn))
