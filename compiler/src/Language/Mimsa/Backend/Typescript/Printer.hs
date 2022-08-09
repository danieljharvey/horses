{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Mimsa.Backend.Typescript.Printer
  ( printModule,
    printLiteral,
    printType,
    printExpr,
    printStatement,
    printTSName,
  )
where

import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Backend.Typescript.DataType
import Language.Mimsa.Backend.Typescript.Types
import Language.Mimsa.Printer

protected :: Set Text
protected = S.fromList ["const", "var", "delete", "default"]

printTSName :: TSName -> Text
printTSName (TSName t) = if S.member t protected then t <> "_" else t

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
  "("
    <> prettyPrint argName
    <> ": "
    <> printType arg
    <> ") => "
    <> printType resp
printType (TSTypeArray as) = printType as <> "[]"
printType (TSTypeTuple as) = "[" <> T.intercalate "," (printType <$> as) <> "]"
printType (TSTypeRecord as) =
  let outputRecordItem (name, val) =
        prettyPrint name <> ": " <> printType val
      items = outputRecordItem <$> M.toList as
   in "{ "
        <> T.intercalate
          ", "
          items
        <> " }"
printType (TSTypeAnd a b) =
  printType a <> " & " <> printType b

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
          (<>) "export " . printStatement
            <$> createConstructorFunctions dt
   in "export type "
        <> prettyPrint tyName
        <> prettyGen
        <> " = "
        <> prettyCons
        <> "; "
        <> prettyConsFns

printLiteral :: TSLiteral -> Text
printLiteral (TSBool True) = "true"
printLiteral (TSBool False) = "false"
printLiteral (TSInt i) = prettyPrint i
printLiteral (TSString str) = "`" <> str <> "`"

printLetBody :: TSLetBody -> Text
printLetBody (TSLetBody (TSBody [] body)) = printExpr body
printLetBody (TSLetBody (TSBody bindings body)) =
  "{ "
    <> mconcat (printStatement <$> bindings)
    <> returnExpr body
    <> " }; "

returnExpr :: TSExpr -> Text
returnExpr tsExpr@TSError {} = printExpr tsExpr <> ";"
returnExpr other = "return " <> printExpr other <> ";"

printStatement :: TSStatement -> Text
printStatement (TSAssignment lhsExpr exprType expr) =
  let tsPrettyType = case exprType of
        Just mt' -> ": " <> printType mt'
        _ -> ""
   in "const "
        <> printExpr lhsExpr
        <> tsPrettyType
        <> " = "
        <> printLetBody expr
        <> "; "
printStatement (TSConditional predicate allBody@(TSLetBody (TSBody [] _))) =
  "if ("
    <> printExpr predicate
    <> ") { return "
    <> printLetBody allBody
    <> "; }; "
printStatement (TSConditional predicate body) =
  "if ("
    <> printExpr predicate
    <> ") "
    <> printLetBody body

printFunctionBody :: TSFunctionBody -> Text
printFunctionBody (TSFunctionBody (TSBody [] body)) = case body of
  TSRecord {} -> "(" <> printExpr body <> ")"
  TSData {} -> "(" <> printExpr body <> ")"
  _ -> printExpr body
printFunctionBody (TSFunctionBody (TSBody bindings body)) =
  "{ "
    <> mconcat (printStatement <$> bindings)
    <> returnExpr body
    <> " }"

printOp :: TSOp -> Text
printOp TSEquals = "==="
printOp TSAdd = "+"
printOp TSSubtract = "-"
printOp TSGreaterThan = ">"
printOp TSGreaterThanOrEqualTo = ">="
printOp TSLessThan = "<"
printOp TSLessThanOrEqualTo = "<="
printOp TSAnd = "&&"
printOp TSStringConcat = "+"

printExpr :: TSExpr -> Text
printExpr (TSLit lit) = printLiteral lit
printExpr (TSFunction name generics mt maybeReturn expr) =
  let prettyGen = case printGeneric <$> S.toList generics of
        [] -> ""
        as -> "<" <> T.intercalate "," as <> ">"
      prettyReturnType = case maybeReturn of
        Just mt' -> ": " <> printType mt'
        _ -> ""
   in prettyGen
        <> "("
        <> printTSName name
        <> ": "
        <> printType mt
        <> ")"
        <> prettyReturnType
        <> " => "
        <> printFunctionBody expr
printExpr (TSVar var) = printTSName var
printExpr (TSApp func val) =
  printExpr func <> "(" <> printExpr val <> ")"
printExpr (TSArray as) =
  "["
    <> T.intercalate
      ","
      (printArrayItem <$> as)
    <> "]"
  where
    printArrayItem (TSArrayItem a) = printExpr a
    printArrayItem (TSArraySpread var) = "..." <> printExpr var
printExpr (TSArrayAccess a expr) =
  printExpr expr <> "[" <> prettyPrint a <> "]"
printExpr (TSInfix op a b) =
  printExpr a
    <> " "
    <> printOp op
    <> " "
    <> printExpr b
printExpr (TSRecord as) =
  let outputRecordItem (name, val) =
        printTSName name <> ": " <> printExpr val
      items = outputRecordItem <$> M.toList as
   in "{ "
        <> T.intercalate
          ", "
          items
        <> " }"
printExpr (TSRecordAccess name expr) =
  printExpr expr <> "." <> printTSName name
printExpr (TSTernary cond thenE elseE) =
  printExpr cond
    <> " ? "
    <> printExpr thenE
    <> " : "
    <> printExpr elseE
printExpr (TSData constructor args) =
  let prettyArgs = T.intercalate "," (printExpr <$> args)
   in "{ type: \"" <> prettyPrint constructor <> "\", vars: [" <> prettyArgs <> "] }"
printExpr (TSError msg) =
  "throw new Error(\"" <> msg <> "\")"
printExpr TSUnderscore = "_"

printModule :: TSModule -> Text
printModule (TSModule dataTypes (TSBody assignments export)) =
  T.intercalate "\n" (printDataType <$> dataTypes)
    <> T.intercalate "\n" (printStatement <$> assignments)
    <> "export const main = "
    <> printExpr export
