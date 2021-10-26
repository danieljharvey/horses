{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Mimsa.Backend.Typescript.Printer
  ( printModule,
    printLiteral,
    printType,
  )
where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Backend.Typescript.DataType
import Language.Mimsa.Backend.Typescript.Types
import Language.Mimsa.Printer

{- maybe these shouldn't be typeclass instances at all? -}
printGeneric :: TSGeneric -> Text
printGeneric (TSGeneric t) = t

printType :: TSType -> Text
printType (TSTypeVar name) = prettyPrint name
printType (TSType namespace name as) =
  ns <> prettyPrint name <> generics
  where
    ns = case namespace of
      Just typeName -> prettyPrint typeName <> "."
      _ -> ""
    generics =
      case as of
        [] -> ""
        typeVar -> "<" <> T.intercalate "," (printType <$> typeVar) <> ">"
printType (TSTypeFun argName arg resp) =
  "(" <> prettyPrint argName <> ": " <> printType arg
    <> ") => "
    <> printType resp
printType (TSTypeArray as) = printType as <> "[]"

printConstructor :: TSConstructor -> Text
printConstructor (TSConstructor tyCon types) =
  "{ type: \"" <> prettyPrint tyCon <> "\", vars: [" <> T.intercalate ", " (printType <$> types) <> "] }"

printDataType :: TSDataType -> Text
printDataType dt@(TSDataType tyName generics constructors) =
  let prettyGen = case generics of
        [] -> ""
        as -> "<" <> T.intercalate ", " (prettyPrint <$> as) <> ">"
      prettyCons = case constructors of
        [] -> "never"
        as -> T.intercalate " | " (printConstructor <$> as)
      prettyConsFns =
        mconcat $
          (<>) "export " . prettyPrint
            <$> createConstructorFunctions dt
   in "export type " <> prettyPrint tyName <> prettyGen <> " = "
        <> prettyCons
        <> "; "
        <> prettyConsFns

printLiteral :: TSLiteral -> Text
printLiteral (TSBool True) = "true"
printLiteral (TSBool False) = "false"
printLiteral (TSInt i) = prettyPrint i
printLiteral (TSString str) = "\"" <> prettyPrint str <> "\""

instance Printer TSLetBody where
  prettyPrint (TSLetBody (TSBody [] body)) = prettyPrint body
  prettyPrint (TSLetBody (TSBody bindings body)) =
    "{ "
      <> mconcat (prettyPrint <$> bindings)
      <> returnExpr body
      <> " }; "

returnExpr :: TSExpr -> Text
returnExpr tsExpr@TSError {} = prettyPrint tsExpr <> ";"
returnExpr other = "return " <> prettyPrint other <> ";"

instance Printer TSStatement where
  prettyPrint (TSAssignment lhsExpr exprType expr) =
    let tsPrettyType = case exprType of
          Just mt' -> ": " <> printType mt'
          _ -> ""
     in "const " <> prettyPrint lhsExpr <> tsPrettyType <> " = "
          <> prettyPrint expr
          <> "; "
  prettyPrint (TSConditional predicate allBody@(TSLetBody (TSBody [] _))) =
    "if (" <> prettyPrint predicate <> ") { return "
      <> prettyPrint allBody
      <> "; }; "
  prettyPrint (TSConditional predicate body) =
    "if (" <> prettyPrint predicate <> ") "
      <> prettyPrint body

instance Printer TSFunctionBody where
  prettyPrint (TSFunctionBody (TSBody [] body)) = case body of
    TSRecord {} -> "(" <> prettyPrint body <> ")"
    TSData {} -> "(" <> prettyPrint body <> ")"
    _ -> prettyPrint body
  prettyPrint (TSFunctionBody (TSBody bindings body)) =
    "{ "
      <> mconcat (prettyPrint <$> bindings)
      <> returnExpr body
      <> " }"

instance Printer TSOp where
  prettyPrint TSEquals = "==="
  prettyPrint TSAdd = "+"
  prettyPrint TSSubtract = "-"
  prettyPrint TSGreaterThanOrEqualTo = ">="
  prettyPrint TSAnd = "&&"
  prettyPrint TSStringConcat = "+"

instance Printer TSExpr where
  prettyPrint (TSLit lit) = printLiteral lit
  prettyPrint (TSFunction name generics mt maybeReturn expr) =
    let prettyGen = case printGeneric <$> S.toList generics of
          [] -> ""
          as -> "<" <> T.intercalate "," as <> ">"
        prettyReturnType = case maybeReturn of
          Just mt' -> ": " <> printType mt'
          _ -> ""
     in prettyGen <> "(" <> prettyPrint name <> ": " <> printType mt <> ")" <> prettyReturnType <> " => "
          <> prettyPrint expr
  prettyPrint (TSVar var) = prettyPrint var
  prettyPrint (TSApp func val) =
    prettyPrint func <> "(" <> prettyPrint val <> ")"
  prettyPrint (TSArray as) =
    "["
      <> T.intercalate
        ","
        (printArrayItem <$> as)
      <> "]"
    where
      printArrayItem (TSArrayItem a) = prettyPrint a
      printArrayItem (TSArraySpread var) = "..." <> prettyPrint var
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
  prettyPrint (TSData constructor args) =
    let prettyArgs = T.intercalate "," (prettyPrint <$> args)
     in "{ type: \"" <> prettyPrint constructor <> "\", vars: [" <> prettyArgs <> "] }"
  prettyPrint (TSError msg) =
    "throw new Error(\"" <> msg <> "\")"
  prettyPrint TSUnderscore = "_"

printModule :: TSModule -> Text
printModule (TSModule dataTypes (TSBody assignments export)) =
  T.intercalate "\n" (printDataType <$> dataTypes)
    <> T.intercalate "\n" (prettyPrint <$> assignments)
    <> "export const main = "
    <> prettyPrint export