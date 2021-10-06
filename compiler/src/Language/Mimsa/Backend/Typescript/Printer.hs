{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Mimsa.Backend.Typescript.Printer
  ( printModule,
    printLiteral,
    destructure,
  )
where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Backend.Typescript.Types
import Language.Mimsa.Printer
import Language.Mimsa.Utils (mapWithIndex)

{- maybe these shouldn't be typeclass instances at all? -}
printGeneric :: TSGeneric -> Text
printGeneric (TSGeneric t) = t

printType :: TSType -> Text
printType (TSTypeVar name) = prettyPrint name
printType (TSType name as) = prettyPrint name <> generics
  where
    generics =
      case as of
        [] -> ""
        typeVar -> "<" <> T.intercalate "," (printType <$> typeVar) <> ">"
printType (TSTypeFun argName arg resp) =
  "(" <> prettyPrint argName <> ": " <> printType arg
    <> ") => "
    <> printType resp

printConstructor :: TSConstructor -> Text
printConstructor (TSConstructor name types) =
  "{ type: \"" <> prettyPrint name <> "\", vars: [" <> T.intercalate ", " (printType <$> types) <> "] }"

printDataType :: TSDataType -> Text
printDataType (TSDataType tyName generics constructors) =
  let prettyGen = case generics of
        [] -> ""
        as -> "<" <> T.intercalate ", " (prettyPrint <$> as) <> ">"
      prettyCons = case constructors of
        [] -> "never"
        as -> T.intercalate " | " (printConstructor <$> as)
   in "type " <> prettyPrint tyName <> prettyGen <> " = " <> prettyCons <> "; "

printLiteral :: TSLiteral -> Text
printLiteral (TSBool True) = "true"
printLiteral (TSBool False) = "false"
printLiteral (TSInt i) = prettyPrint i
printLiteral (TSString str) = "\"" <> prettyPrint str <> "\""

-- | these aren't always useful (the pattern lit case, for example)
-- so we'd like to signal that there's nothing to destructure and not to
-- bother, for now we output something useless
destructure :: TSPattern -> Text
destructure (TSPatternVar n) = prettyPrint n
destructure TSPatternWildcard = "_"
destructure (TSPatternPair a b) =
  "[" <> destructure a <> ","
    <> destructure b
    <> "]"
destructure (TSPatternRecord as) =
  let outputRecordItem (name, val) =
        prettyPrint name <> ": " <> destructure val
      items = outputRecordItem <$> M.toList as
   in "{ "
        <> T.intercalate
          ", "
          items
        <> " }"
destructure (TSPatternConstructor _ vars) =
  "{ vars: [" <> T.intercalate ", " (destructure <$> vars) <> "] }"
destructure (TSPatternArray as spread) =
  let tsSpread = case spread of
        TSSpreadValue a -> ", ..." <> prettyPrint a
        _ -> ""
   in "[" <> T.intercalate ", " (destructure <$> as) <> tsSpread <> "]"
destructure (TSPatternLit _) = "_"

conditions :: TSPattern -> TSExpr
conditions pat =
  let parts = toMatchExpression (TSVar "value") pat
   in case parts of
        [] -> TSLit (TSBool True)
        (a : as) -> foldr (TSInfix TSAnd) a as

-- | turn a pattern map into a match expression for this pattern
toMatchExpression :: TSExpr -> TSPattern -> [TSExpr]
toMatchExpression _ TSPatternWildcard =
  mempty
toMatchExpression _ (TSPatternVar _) =
  mempty
toMatchExpression name (TSPatternPair a b) =
  toMatchExpression (TSArrayAccess 0 name) a
    <> toMatchExpression (TSArrayAccess 1 name) b
toMatchExpression name (TSPatternLit lit) =
  [TSInfix TSEquals name (TSLit lit)]
toMatchExpression name (TSPatternRecord items) =
  let subPattern (k, v) = toMatchExpression (TSRecordAccess k name) v
   in mconcat (subPattern <$> M.toList items)
toMatchExpression name (TSPatternConstructor tyCon args) =
  let tyConGuard = TSInfix TSEquals (TSRecordAccess "type" name) (TSLit (TSString (prettyPrint tyCon)))
      subPattern i a = toMatchExpression (TSArrayAccess (i - 1) (TSRecordAccess "vars" name)) a
   in [tyConGuard] <> mconcat (mapWithIndex subPattern args)
toMatchExpression name (TSPatternArray as spread) =
  let lengthGuard = case spread of
        TSNoSpread ->
          TSInfix
            TSEquals
            (TSRecordAccess "length" name)
            (TSLit (TSInt (length as)))
        TSSpreadWildcard ->
          TSInfix
            TSGreaterThanOrEqualTo
            (TSRecordAccess "length" name)
            (TSLit (TSInt (length as)))
        (TSSpreadValue _) ->
          TSInfix
            TSGreaterThanOrEqualTo
            (TSRecordAccess "length" name)
            (TSLit (TSInt (length as)))
      subPattern i a =
        toMatchExpression (TSArrayAccess (i - 1) name) a
   in [lengthGuard] <> mconcat (mapWithIndex subPattern as)

{-
toMatchExpression name (TSString a as) =
  let lengthGuard = GreaterThanOrEQ (name <> ".length") "1"
      aValue = case a of
        StrValue _ vA -> M.singleton vA (name <> ".charAt(0)")
        _ -> mempty
      asValue = case as of
        StrValue _ vAs -> M.singleton vAs (name <> ".slice(1)")
        _ -> mempty
   in [lengthGuard]
-}

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
  prettyPrint (TSAssignment pat expr) =
    "const " <> destructure pat <> " = " <> prettyPrint expr <> "; "
  prettyPrint (TSConditional predicate allBody@(TSLetBody (TSBody [] _))) =
    "if (" <> prettyPrint (conditions predicate) <> ") { return "
      <> prettyPrint allBody
      <> " }; "
  prettyPrint (TSConditional predicate body) =
    "if (" <> prettyPrint (conditions predicate) <> ") "
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
  prettyPrint TSMinus = "-"
  prettyPrint TSGreaterThanOrEqualTo = ">="
  prettyPrint TSAnd = "&&"

instance Printer TSExpr where
  prettyPrint (TSLit lit) = printLiteral lit
  prettyPrint (TSFunction name generics mt expr) =
    let prettyGen = case printGeneric <$> S.toList generics of
          [] -> ""
          as -> "<" <> T.intercalate "," as <> ">"
     in prettyGen <> "(" <> prettyPrint name <> ": " <> printType mt <> ") => "
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
  prettyPrint (TSData constructor args) =
    let prettyArgs = T.intercalate "," (prettyPrint <$> args)
     in "{ type: \"" <> prettyPrint constructor <> "\", vars: [" <> prettyArgs <> "] }"
  prettyPrint (TSError msg) =
    "throw new Error(\"" <> msg <> "\")"

printModule :: TSModule -> Text
printModule (TSModule dataTypes (TSBody assignments export)) =
  T.intercalate "\n" (printDataType <$> dataTypes)
    <> T.intercalate "\n" (prettyPrint <$> assignments)
    <> "export const main = "
    <> prettyPrint export
