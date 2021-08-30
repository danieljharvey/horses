{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Backend.Typescript.Types where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Printer
import Language.Mimsa.Types.Identifiers.Name
import Language.Mimsa.Types.Identifiers.TyCon

data TSType = TSType Text [TSType] | TSTypeVar Text
  deriving stock (Eq, Ord, Show)

instance Printer TSType where
  prettyPrint (TSTypeVar name) = prettyPrint name
  prettyPrint (TSType name as) = prettyPrint name <> generics
    where
      generics =
        case as of
          [] -> ""
          typeVar -> "<" <> T.intercalate "," (prettyPrint <$> typeVar) <> ">"

data TSLiteral = TSBool Bool | TSString Text | TSInt Int
  deriving stock (Eq, Ord, Show)

instance Printer TSLiteral where
  prettyPrint (TSBool True) = "true"
  prettyPrint (TSBool False) = "false"
  prettyPrint (TSInt i) = prettyPrint i
  prettyPrint (TSString str) = "\"" <> prettyPrint str <> "\""

data TSPattern
  = TSPatternVar Name
  | TSPatternPair TSPattern TSPattern
  | TSPatternRecord (Map Name TSPattern)
  | TSPatternConstructor TyCon [TSPattern]
  | TSPatternWildcard
  deriving stock (Eq, Ord, Show)

instance Printer TSPattern where
  prettyPrint (TSPatternVar n) = prettyPrint n
  prettyPrint TSPatternWildcard = "_"
  prettyPrint (TSPatternPair a b) =
    "[" <> prettyPrint a <> ","
      <> prettyPrint b
      <> "]"
  prettyPrint (TSPatternRecord as) =
    let outputRecordItem (name, val) =
          prettyPrint name <> ": " <> prettyPrint val
        items = outputRecordItem <$> M.toList as
     in "{ "
          <> T.intercalate
            ", "
            items
          <> " }"
  prettyPrint (TSPatternConstructor _ vars) =
    "{ vars: [" <> T.intercalate ", " (prettyPrint <$> vars) <> "] }"

newtype TSLetBody = TSLetBody TSBody
  deriving newtype (Eq, Ord, Show)

instance Printer TSLetBody where
  prettyPrint (TSLetBody (TSBody [] body)) = prettyPrint body
  prettyPrint (TSLetBody (TSBody _ _)) = undefined

data TSAssignment = TSAssignment TSPattern TSLetBody
  deriving stock (Eq, Ord, Show)

instance Printer TSAssignment where
  prettyPrint (TSAssignment name expr) =
    "const " <> prettyPrint name <> " = " <> prettyPrint expr

-- this could be top level or in a function body, it's a list of
-- assignments followed by either the return or an export
-- won't be prettyprinted directly as it depends on context
data TSBody = TSBody [TSAssignment] TSExpr
  deriving stock (Eq, Ord, Show)

newtype TSFunctionBody = TSFunctionBody TSBody
  deriving newtype (Eq, Ord, Show)

instance Printer TSFunctionBody where
  prettyPrint (TSFunctionBody (TSBody [] body)) = prettyPrint body
  prettyPrint (TSFunctionBody (TSBody bindings body)) =
    "{ "
      <> mconcat ((<> "; ") . prettyPrint <$> bindings)
      <> "return "
      <> prettyPrint body
      <> " }"

data TSOp
  = TSEquals
  | TSAdd
  | TSMinus
  | TSGreaterThanOrEqualTo
  deriving stock (Eq, Ord, Show)

instance Printer TSOp where
  prettyPrint TSEquals = "==="
  prettyPrint TSAdd = "+"
  prettyPrint TSMinus = "-"
  prettyPrint TSGreaterThanOrEqualTo = ">="

data TSExpr
  = TSLit TSLiteral
  | TSFunction Name TSType TSFunctionBody
  | TSRecord
      (Map Name TSExpr)
  | TSRecordAccess Name TSExpr
  | TSArray
      [TSExpr]
  | TSArrayAccess Int TSExpr
  | TSVar Name
  | TSApp TSExpr TSExpr
  | TSInfix TSOp TSExpr TSExpr
  | TSTernary TSExpr TSExpr TSExpr
  deriving stock (Eq, Ord, Show)

instance Printer TSExpr where
  prettyPrint (TSLit lit) = prettyPrint lit
  prettyPrint (TSFunction name mt expr) =
    "(" <> prettyPrint name <> ": " <> prettyPrint mt <> ") => "
      <> prettyPrint expr
  prettyPrint (TSVar var) = prettyPrint var
  prettyPrint (TSApp func val) =
    prettyPrint func <> "(" <> prettyPrint val <> ")"
  prettyPrint (TSArray as) =
    "["
      <> T.intercalate
        ","
        (prettyPrint <$> as)
      <> "]"
  prettyPrint (TSArrayAccess a expr) =
    prettyPrint expr <> "[" <> prettyPrint a <> "]"
  prettyPrint (TSInfix op a b) =
    prettyPrint a <> " "
      <> prettyPrint op
      <> " "
      <> prettyPrint b
  prettyPrint (TSRecord as) =
    let outputRecordItem (name, val) =
          prettyPrint name <> ": " <> prettyPrint val
        items = outputRecordItem <$> M.toList as
     in "{ "
          <> T.intercalate
            ", "
            items
          <> " }"
  prettyPrint (TSRecordAccess name expr) =
    prettyPrint expr <> "." <> prettyPrint name
  prettyPrint (TSTernary cond thenE elseE) =
    prettyPrint cond <> " ? " <> prettyPrint thenE <> " : "
      <> prettyPrint elseE

newtype TSModule = TSModule TSBody
  deriving newtype (Eq, Ord, Show)

instance Printer TSModule where
  prettyPrint (TSModule (TSBody assignments export)) =
    mconcat ((<> "\n") . prettyPrint <$> assignments)
      <> "export const main = "
      <> prettyPrint export
