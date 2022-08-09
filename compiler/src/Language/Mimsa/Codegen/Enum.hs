{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Codegen.Enum
  ( toString,
    fromString,
  )
where

import Data.Coerce
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error.CodegenError
import Language.Mimsa.Types.Identifiers

-- | An enum is a datatype with at least one constructor
-- | each have no arguments
toString ::
  DataType ->
  Either CodegenError (Expr Name ())
toString (DataType typeName [] items) = do
  let tyName = typeNameToName typeName
  let createMatch (consName, vars) =
        case vars of
          [] ->
            Right
              ( PConstructor mempty Nothing consName mempty,
                str (showTyCon consName)
              )
          _ ->
            Left $
              ConstructorShouldHaveNoArgs consName
  matches <- traverse createMatch (M.toList items)
  case NE.nonEmpty matches of
    Nothing -> Left NoConstructorMatches
    Just _ ->
      Right
        ( MyLambda
            mempty
            (Identifier mempty tyName)
            ( MyPatternMatch
                mempty
                (MyVar mempty Nothing tyName)
                matches
            )
        )
toString _ = Left TypeShouldHaveNoVariables

fromString ::
  DataType ->
  Either CodegenError (Expr Name ())
fromString (DataType _ [] items) = do
  let tyName = "str"
  let createMatch :: (TyCon, [a]) -> Either CodegenError (Pattern Name (), Expr Name ())
      createMatch (consName, vars) =
        case vars of
          [] ->
            Right
              ( PLit mempty (MyString (coerce consName)),
                MyApp
                  mempty
                  (MyConstructor mempty Nothing "Just")
                  (MyConstructor mempty Nothing consName)
              )
          _ ->
            Left $ ConstructorShouldHaveNoArgs consName
  matches <- traverse createMatch (M.toList items)
  let empty = (PWildcard mempty, MyConstructor mempty Nothing "Nothing")
  case NE.nonEmpty matches of
    Nothing -> Left NoConstructorMatches
    Just _ ->
      Right
        ( MyLambda
            mempty
            (Identifier mempty tyName)
            ( MyPatternMatch
                mempty
                (MyVar mempty Nothing tyName)
                (matches <> [empty])
            )
        )
fromString _ = Left TypeShouldHaveNoVariables

str :: (Monoid ann) => StringType -> Expr a ann
str a = MyLiteral mempty (MyString a)

showTyCon :: TyCon -> StringType
showTyCon (TyCon t) = StringType t
