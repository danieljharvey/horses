{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Typechecker.Codegen.Enum
  ( toString,
  )
where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Text (Text)
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers

-- | An enum is a datatype with at least one constructor
-- | each have no arguments
-- | TODO: create fromString once we have decided how to include
-- | datatypes in the stdLib
toString ::
  DataType ->
  Either Text (Expr Name ())
toString (DataType tyCon [] items) = do
  let tyName = tyConToName tyCon
  let createMatch (consName, vars) =
        case vars of
          [] -> Right (consName, str (showTyCon consName))
          _ ->
            Left $
              "Constructor "
                <> prettyPrint consName
                <> " is expected to have no arguments"
  matches <- traverse createMatch (M.toList items)
  case NE.nonEmpty matches of
    Nothing -> Left "Type has no constructors"
    Just neMatches ->
      Right
        ( MyLambda
            mempty
            tyName
            ( MyCaseMatch
                mempty
                (MyVar mempty tyName)
                neMatches
                Nothing
            )
        )
toString _ = Left "Datatype is expected to have no parameters"

str :: (Monoid ann) => StringType -> Expr a ann
str a = MyLiteral mempty (MyString a)

showTyCon :: TyCon -> StringType
showTyCon (TyCon t) = StringType t
