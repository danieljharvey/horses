{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Codegen.Enum
  ( toString,
    fromString,
  )
where

import Data.Coerce
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Text (Text)
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers

-- | An enum is a datatype with at least one constructor
-- | each have no arguments
toString ::
  DataType () ->
  Either Text (Expr Name ())
toString (DataType tyCon [] items) = do
  let tyName = tyConToName tyCon
  let createMatch (consName, vars) =
        case vars of
          [] ->
            Right
              ( PConstructor mempty consName mempty,
                str (showTyCon consName)
              )
          _ ->
            Left $
              "Constructor "
                <> prettyPrint consName
                <> " is expected to have no arguments"
  matches <- traverse createMatch (M.toList items)
  case NE.nonEmpty matches of
    Nothing -> Left "Type has no constructors"
    Just _ ->
      Right
        ( MyLambda
            mempty
            tyName
            ( MyPatternMatch
                mempty
                (MyVar mempty tyName)
                matches
            )
        )
toString _ = Left "Datatype is expected to have no parameters"

fromString ::
  DataType () ->
  Either Text (Expr Name ())
fromString (DataType _ [] items) = do
  let tyName = "str"
  let createMatch :: (TyCon, [a]) -> Either Text (Pattern Name (), Expr Name ())
      createMatch (consName, vars) =
        case vars of
          [] ->
            Right
              ( PLit mempty (MyString (coerce consName)),
                MyConsApp
                  mempty
                  (MyConstructor mempty "Just")
                  (MyConstructor mempty consName)
              )
          _ ->
            Left $
              "Constructor "
                <> prettyPrint consName
                <> " is expected to have no arguments"
  matches <- traverse createMatch (M.toList items)
  let empty = (PWildcard mempty, MyConstructor mempty "Nothing")
  case NE.nonEmpty matches of
    Nothing -> Left "Type has no constructors"
    Just _ ->
      Right
        ( MyLambda
            mempty
            tyName
            ( MyPatternMatch
                mempty
                (MyVar mempty tyName)
                (matches <> [empty])
            )
        )
fromString _ = Left "Datatype is expected to have no parameters"

str :: (Monoid ann) => StringType -> Expr a ann
str a = MyLiteral mempty (MyString a)

showTyCon :: TyCon -> StringType
showTyCon (TyCon t) = StringType t
