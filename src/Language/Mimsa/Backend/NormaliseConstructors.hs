{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Backend.NormaliseConstructors where

import Data.Foldable (foldl')
import qualified Data.Map as M
import Language.Mimsa.Printer
import Language.Mimsa.Project.Persistence
import Language.Mimsa.Store.ResolvedDeps
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedTypeDeps

-- turns Constructors into functions
normaliseConstructors :: Project Annotation -> Expr Name () -> Expr Name ()
normaliseConstructors project (MyConstructor _ tyCon) = constructorToFunction project tyCon
normaliseConstructors _ a = a

typeNameToName :: Int -> TypeName -> Name
typeNameToName _ (VarName name) = name
typeNameToName i _ = mkName $ "U" <> prettyPrint i

constructorToFunction :: Project Annotation -> TyCon -> Expr Name ()
constructorToFunction project tyCon =
  let tyVars = extractTypeConstructor tyCon <$> findDataTypeInProject project tyCon
   in case tyVars of
        Just [] -> MyConstructor mempty tyCon
        Just as ->
          let numberList = zip [1 ..] as
              withConsApp =
                foldl'
                  ( \expr' (i, tn) ->
                      let variable = typeNameToName i tn
                       in MyConsApp mempty expr' (MyVar mempty variable)
                  )
                  (MyConstructor mempty tyCon)
                  numberList
           in foldr
                ( \(i, tn) expr' ->
                    let variable = typeNameToName i tn
                     in MyLambda mempty variable expr'
                )
                withConsApp
                numberList
        _ -> MyConstructor mempty tyCon

findDataTypeInProject :: Project Annotation -> TyCon -> Maybe DataType
findDataTypeInProject project tyCon =
  case resolveTypeDeps (store project) (getCurrentTypeBindings $ typeBindings project) of
    Right (ResolvedTypeDeps dataTypes) ->
      snd <$> M.lookup tyCon dataTypes
    Left _ -> Nothing

extractTypeConstructor :: TyCon -> DataType -> [TypeName]
extractTypeConstructor tc dt =
  case M.lookup tc (dtConstructors dt) of
    Just names -> names
    _ -> []
