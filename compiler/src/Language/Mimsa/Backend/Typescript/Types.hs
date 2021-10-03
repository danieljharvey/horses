{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Backend.Typescript.Types where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Printer
import Language.Mimsa.Types.Identifiers.Name
import Language.Mimsa.Types.Identifiers.TyCon
import Language.Mimsa.Utils (mapWithIndex)

-- | which generics have been used already?
newtype TSGeneric = TSGeneric Text
  deriving newtype (Eq, Ord, Show)

instance Printer TSGeneric where
  prettyPrint (TSGeneric t) = t

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

data TSSpread
  = TSNoSpread
  | TSSpreadWildcard
  | TSSpreadValue Name
  deriving stock (Eq, Ord, Show)

data TSPattern
  = TSPatternVar Name
  | TSPatternPair TSPattern TSPattern
  | TSPatternRecord (Map Name TSPattern)
  | TSPatternConstructor TyCon [TSPattern]
  | TSPatternLit TSLiteral
  | TSPatternArray [TSPattern] TSSpread
  | TSPatternWildcard
  deriving stock (Eq, Ord, Show)

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
  let parts = toPatternMap (TSVar "value") pat
   in case parts of
        [] -> TSLit (TSBool True)
        (a : as) -> foldr (TSInfix TSAnd) a as

toPatternMap :: TSExpr -> TSPattern -> [TSExpr]
toPatternMap _ TSPatternWildcard =
  mempty
toPatternMap _ (TSPatternVar _) =
  mempty
toPatternMap name (TSPatternPair a b) =
  toPatternMap (TSArrayAccess 0 name) a
    <> toPatternMap (TSArrayAccess 1 name) b
toPatternMap name (TSPatternLit lit) =
  [TSInfix TSEquals name (TSLit lit)]
toPatternMap name (TSPatternRecord items) =
  let subPattern (k, v) = toPatternMap (TSRecordAccess k name) v
   in mconcat (subPattern <$> M.toList items)
toPatternMap name (TSPatternConstructor tyCon args) =
  let tyConGuard = TSInfix TSEquals (TSRecordAccess "type" name) (TSLit (TSString (prettyPrint tyCon)))
      subPattern i a = toPatternMap (TSArrayAccess (i - 1) (TSRecordAccess "vars" name)) a
   in [tyConGuard] <> mconcat (mapWithIndex subPattern args)
toPatternMap name (TSPatternArray as spread) =
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
        toPatternMap (TSArrayAccess (i - 1) name) a
   in [lengthGuard] <> mconcat (mapWithIndex subPattern as)

{-
toPatternMap name (TSString a as) =
  let lengthGuard = GreaterThanOrEQ (name <> ".length") "1"
      aValue = case a of
        StrValue _ vA -> M.singleton vA (name <> ".charAt(0)")
        _ -> mempty
      asValue = case as of
        StrValue _ vAs -> M.singleton vAs (name <> ".slice(1)")
        _ -> mempty
   in [lengthGuard]
-}

newtype TSLetBody = TSLetBody TSBody
  deriving newtype (Eq, Ord, Show)

instance Printer TSLetBody where
  prettyPrint (TSLetBody (TSBody [] body)) = prettyPrint body
  prettyPrint (TSLetBody (TSBody bindings body)) =
    "{\n"
      <> mconcat ((<> "\n") . prettyPrint <$> bindings)
      <> returnExpr body
      <> "\n}"

returnExpr :: TSExpr -> Text
returnExpr tsExpr@TSError {} = prettyPrint tsExpr
returnExpr other = "return " <> prettyPrint other

data TSStatement
  = TSAssignment TSPattern TSLetBody
  | TSConditional TSPattern TSLetBody
  deriving stock (Eq, Ord, Show)

instance Printer TSStatement where
  prettyPrint (TSAssignment pat expr) =
    "const " <> destructure pat <> " = " <> prettyPrint expr <> "\n"
  prettyPrint (TSConditional predicate allBody@(TSLetBody (TSBody [] _))) =
    "if (" <> prettyPrint (conditions predicate) <> ") {\nreturn "
      <> prettyPrint allBody
      <> "\n}"
  prettyPrint (TSConditional predicate body) =
    "if (" <> prettyPrint (conditions predicate) <> ") "
      <> prettyPrint body

-- this could be top level or in a function body, it's a list of
-- assignments followed by either the return or an export
-- won't be prettyprinted directly as it depends on context
data TSBody = TSBody [TSStatement] TSExpr
  deriving stock (Eq, Ord, Show)

newtype TSFunctionBody = TSFunctionBody TSBody
  deriving newtype (Eq, Ord, Show)

instance Printer TSFunctionBody where
  prettyPrint (TSFunctionBody (TSBody [] body)) = prettyPrint body
  prettyPrint (TSFunctionBody (TSBody bindings body)) =
    "{\n"
      <> mconcat ((<> "\n") . prettyPrint <$> bindings)
      <> returnExpr body
      <> "\n}"

data TSOp
  = TSEquals
  | TSAdd
  | TSMinus
  | TSGreaterThanOrEqualTo
  | TSAnd
  deriving stock (Eq, Ord, Show)

instance Printer TSOp where
  prettyPrint TSEquals = "==="
  prettyPrint TSAdd = "+"
  prettyPrint TSMinus = "-"
  prettyPrint TSGreaterThanOrEqualTo = ">="
  prettyPrint TSAnd = "&&"

data TSExpr
  = TSLit TSLiteral
  | TSFunction Name (Set TSGeneric) TSType TSFunctionBody
  | TSRecord (Map Name TSExpr)
  | TSRecordAccess Name TSExpr
  | TSArray [TSExpr]
  | TSArrayAccess Int TSExpr
  | TSVar Name
  | TSApp TSExpr TSExpr
  | TSInfix TSOp TSExpr TSExpr
  | TSTernary TSExpr TSExpr TSExpr
  | TSData Text [TSExpr]
  | TSError Text
  deriving stock (Eq, Ord, Show)

instance Printer TSExpr where
  prettyPrint (TSLit lit) = prettyPrint lit
  prettyPrint (TSFunction name generics mt expr) =
    let prettyGen = case prettyPrint <$> S.toList generics of
          [] -> ""
          as -> "<" <> T.intercalate "," as <> ">"
     in prettyGen <> "(" <> prettyPrint name <> ": " <> prettyPrint mt <> ") => "
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

newtype TSModule = TSModule TSBody
  deriving newtype (Eq, Ord, Show)

instance Printer TSModule where
  prettyPrint (TSModule (TSBody assignments export)) =
    mconcat ((<> "\n") . prettyPrint <$> assignments)
      <> "export const main = "
      <> prettyPrint export
