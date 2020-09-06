{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Printer
  ( Printer (..),
    renderWithWidth,
  )
where

-- the Printer type class is used for internal debugging
-- instances will mostly use the Prettyprinter render functions

-- for code output etc it is better to combine the render function output
-- directly so that the whole lot is arranged together

import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Language.Mimsa.Printer.Construct
import Language.Mimsa.Printer.MonoType
import Language.Mimsa.Printer.Name
import Language.Mimsa.Printer.TypeError
import Language.Mimsa.Printer.Variable
import Language.Mimsa.Types

renderWithWidth :: Int -> Doc ann -> Text
renderWithWidth w doc = renderStrict (layoutPretty layoutOptions (unAnnotate doc))
  where
    layoutOptions = LayoutOptions {layoutPageWidth = AvailablePerLine w 1}

class Printer a where
  prettyPrint :: a -> Text
  default prettyPrint :: (Show a) => a -> Text
  prettyPrint = T.pack . show

instance Printer Text where
  prettyPrint a = a

instance Printer Int where
  prettyPrint = T.pack . show

instance (Printer a) => Printer [a] where
  prettyPrint as = T.intercalate ", " (prettyPrint <$> as)

instance (Printer a, Printer b) => Printer (a, b) where
  prettyPrint (a, b) = "\n " <> T.intercalate "\n " [prettyPrint a, prettyPrint b]

instance (Printer k, Printer v) => Printer (Map k v) where
  prettyPrint map' =
    let printRow (k, v) = prettyPrint k <> ": " <> prettyPrint v
     in T.intercalate ", " (printRow <$> M.toList map')

instance (Printer a, Printer b, Printer c) => Printer (a, b, c) where
  prettyPrint (a, b, c) =
    "\n "
      <> T.intercalate
        "\n "
        [prettyPrint a, prettyPrint b, prettyPrint c]

instance (Printer a, Printer b, Printer c, Printer d) => Printer (a, b, c, d) where
  prettyPrint (a, b, c, d) =
    "\n "
      <> T.intercalate
        "\n "
        [prettyPrint a, prettyPrint b, prettyPrint c, prettyPrint d]

instance (Printer a) => Printer (Set a) where
  prettyPrint as = "[" <> T.intercalate ", " (prettyPrint <$> S.toList as) <> "]"

----

instance Printer MonoType where
  prettyPrint = renderWithWidth 40 . renderMonoType

----

instance Printer Name where
  prettyPrint = renderWithWidth 40 . renderName

----

instance Printer BiIds where
  prettyPrint NoId = "-"
  prettyPrint (OneId v1) = prettyPrint v1
  prettyPrint (TwoIds v1 v2) = T.intercalate ", " (prettyPrint <$> [v1, v2])
  prettyPrint (ThreeIds v1 v2 v3) = T.intercalate ", " (prettyPrint <$> [v1, v2, v3])

instance Printer Variable where
  prettyPrint = renderWithWidth 40 . renderVariable

----

instance Printer Construct where
  prettyPrint = renderWithWidth 40 . renderConstruct

----

instance Printer StringType where
  prettyPrint (StringType s) = s

--------

instance Printer FuncName where
  prettyPrint (FuncName a) = a

-------

instance Printer Literal where
  prettyPrint (MyInt i) = T.pack (show i)
  prettyPrint (MyBool True) = "True"
  prettyPrint (MyBool False) = "False"
  prettyPrint (MyString str) = "\"" <> prettyPrint str <> "\""
  prettyPrint MyUnit = "Unit"

-------

instance Printer DataType where
  prettyPrint (DataType name' vars' constructors') =
    "type " <> prettyPrint name'
      <> printVars vars'
      <> if M.null constructors'
        then " "
        else
          " = "
            <> T.intercalate " | " (printCons <$> M.toList constructors')
    where
      printVars [] = ""
      printVars as = " " <> T.intercalate " " (prettyPrint <$> as)
      printCons (consName, args) =
        prettyPrint consName <> " " <> T.intercalate " " (prettyPrint <$> args)

-------

instance (Printer a) => Printer (Expr a) where
  prettyPrint (MyLiteral l) = prettyPrint l
  prettyPrint (MyVar var) = prettyPrint var
  prettyPrint (MyLet var expr1 expr2) =
    "let " <> prettyPrint var
      <> " = "
      <> printSubExpr expr1
      <> " in "
      <> printSubExpr expr2
  prettyPrint (MyLetPair var1 var2 expr1 body) =
    "let (" <> prettyPrint var1 <> ", " <> prettyPrint var2
      <> ") = "
      <> printSubExpr expr1
      <> " in "
      <> printSubExpr body
  prettyPrint (MyLambda binder expr) =
    "\\"
      <> prettyPrint binder
      <> " -> "
      <> printSubExpr expr
  prettyPrint (MyApp (MyApp (MyApp func arg1) arg2) arg3) =
    printSubExpr func <> "("
      <> printSubExpr arg1
      <> ")("
      <> printSubExpr arg2
      <> ")("
      <> printSubExpr arg3
      <> ")"
  prettyPrint (MyApp (MyApp func arg1) arg2) =
    printSubExpr func <> "("
      <> printSubExpr arg1
      <> ")("
      <> printSubExpr arg2
      <> ")"
  prettyPrint (MyApp func arg) =
    printSubExpr func <> "("
      <> printSubExpr arg
      <> ")"
  prettyPrint (MyRecordAccess expr name) =
    printSubExpr expr <> "." <> prettyPrint name
  prettyPrint (MyIf if' then' else') =
    "if "
      <> printSubExpr if'
      <> " then "
      <> printSubExpr then'
      <> " else "
      <> printSubExpr else'
  prettyPrint (MyPair a b) =
    "("
      <> printSubExpr a
      <> ", "
      <> printSubExpr b
      <> ")"
  prettyPrint (MyRecord map') = "{" <> T.intercalate ", " exprs' <> "}"
    where
      exprs' =
        ( \(name, val) ->
            prettyPrint name
              <> ": "
              <> printSubExpr val
        )
          <$> M.toList map'
  prettyPrint (MyData dataType expr) =
    prettyPrint dataType
      <> " in "
      <> printSubExpr expr
  prettyPrint (MyConstructor name) = prettyPrint name
  prettyPrint (MyConsApp fn val) = prettyPrint fn <> " " <> printSubExpr val
  prettyPrint (MyCaseMatch sumExpr matches catchAll) =
    "case "
      <> printSubExpr sumExpr
      <> " of "
      <> T.intercalate " | " (printMatch <$> NE.toList matches)
      <> maybe "" (\catchExpr -> " | otherwise " <> printSubExpr catchExpr) catchAll
    where
      printMatch (construct, expr') =
        prettyPrint construct <> " " <> printSubExpr expr'

inParens :: (Printer a) => a -> Text
inParens a = "(" <> prettyPrint a <> ")"

-- print simple things with no brackets, and complex things inside brackets
printSubExpr :: (Printer a) => Expr a -> Text
printSubExpr expr = case expr of
  all'@MyLet {} -> inParens all'
  all'@MyLambda {} -> inParens all'
  all'@MyRecord {} -> inParens all'
  all'@MyIf {} -> inParens all'
  all'@MyConstructor {} -> inParens all'
  all'@MyConsApp {} -> inParens all'
  all'@MyPair {} -> inParens all'
  a -> prettyPrint a

-----------------

instance Printer TypeName where
  prettyPrint (ConsName c []) = prettyPrint c
  prettyPrint (ConsName c tys) = "(" <> prettyPrint c <> vars <> ")"
    where
      vars =
        " " <> T.intercalate " " (prettyPrint <$> tys)
  prettyPrint (VarName v) = prettyPrint v

-----

instance Printer UniVar where
  prettyPrint (UniVar a) = T.pack . show $ a

-----

instance Printer Bindings where
  prettyPrint (Bindings b) = "{ " <> T.intercalate ", " (prettyPrint <$> M.keys b) <> " }"

-----

instance Printer Environment where
  prettyPrint (Environment typeSchemes _dataTypes) =
    "[\n"
      <> T.intercalate ", \n" (printRow <$> M.toList typeSchemes)
      <> "\n]"
    where
      printRow (var, scheme) =
        prettyPrint var <> ": " <> prettyPrint scheme

---

instance Printer Error where
  prettyPrint (TypeErr t) = "TypeError: " <> prettyPrint t
  prettyPrint (ResolverErr a) = "ResolverError: " <> prettyPrint a
  prettyPrint (InterpreterErr a) = "InterpreterError: " <> prettyPrint a
  prettyPrint (UsageErr a) = "UsageError: " <> prettyPrint a
  prettyPrint (ParseErr a) = "ParseError: " <> a
  prettyPrint (OtherError a) = "OtherError: " <> a

----

instance Printer ExprHash where
  prettyPrint (ExprHash a) = T.pack . show $ a

------

instance Printer InterpreterError where
  prettyPrint (CouldNotFindVar _ name) = "Could not find var " <> prettyPrint name
  prettyPrint (CouldNotFindBuiltIn _ name) = "Could not find built-in " <> prettyPrint name
  prettyPrint (CannotDestructureAsPair expr) = "Expected a pair. Cannot destructure: " <> prettyPrint expr
  prettyPrint (CannotDestructureAsSum expr) = "Expected a sum type. Cannot destructure: " <> prettyPrint expr
  prettyPrint (CannotDestructureAsRecord expr name) = "Expected a record with a member " <> prettyPrint name <> ". Cannot destructure: " <> prettyPrint expr
  prettyPrint (CannotDestructureAsList expr) = "Expected a list. Cannot destructure: " <> prettyPrint expr
  prettyPrint (CannotApplyToNonFunction expr) = "Expected a function. Cannot apply a value to " <> prettyPrint expr
  prettyPrint (CannotFindMemberInRecord items name) = "Could not find member " <> prettyPrint name <> " in " <> itemList
    where
      itemList = "[ " <> T.intercalate ", " (prettyPrint <$> M.keys items) <> " ]"
  prettyPrint (PredicateForIfMustBeABoolean expr) = "Expected a boolean as a predicate. Cannot use: " <> prettyPrint expr
  prettyPrint (CouldNotUnwrapBuiltIn name) = "Could unwrap built-in " <> prettyPrint name
  prettyPrint (CouldNotMatchBuiltInId ids) = "Could not match built in ids " <> prettyPrint ids
  prettyPrint (PatternMatchFailure expr') = "Could not pattern match on value " <> prettyPrint expr'
  prettyPrint (SelfReferencingBinding b) = "Could not bind variable " <> prettyPrint b <> " to itself."
  prettyPrint UnknownInterpreterError = "Unknown interpreter error"

-----

instance Printer ResolvedDeps where
  prettyPrint (ResolvedDeps deps) =
    "{"
      <> T.intercalate ", " (prettyPrint <$> M.keys deps)
      <> "}"

-----

instance Printer ResolverError where
  prettyPrint (MissingBinding name (Bindings bindings')) = "A binding for " <> prettyPrint name <> " could not be found in " <> bindingsList
    where
      bindingsList = "[ " <> T.intercalate ", " (prettyPrint <$> M.keys bindings') <> " ]"

-----

instance Printer Scheme where
  prettyPrint (Scheme vars mt) = varText <> prettyPrint mt
    where
      varText = case vars of
        [] -> ""
        a -> "[" <> T.intercalate ", " (prettyPrint <$> a) <> "] "

-----

instance Printer Scope where
  prettyPrint (Scope s) = "{ " <> T.intercalate ", " (printItem <$> M.toList s) <> " }"
    where
      printItem (k, a) = prettyPrint k <> ": " <> prettyPrint a

-----

instance Printer Substitutions where
  prettyPrint (Substitutions s1) = "\n  " <> T.intercalate "\n  " (printRow <$> M.toList s1)
    where
      printRow (var, mt) = prettyPrint var <> ": " <> prettyPrint mt

-----

instance Printer TypeConstructor where
  prettyPrint (TypeConstructor consName _tyTypeVars consTypes) =
    prettyPrint consName <> " " <> T.intercalate " " (prettyPrint <$> consTypes)

-----

instance Printer TypeError where
  prettyPrint = renderWithWidth 40 . align . vcat . renderTypeError

-----

instance Printer Usage where
  prettyPrint (Transient name _) =
    "Transient dependency of "
      <> prettyPrint name
  prettyPrint (Direct name _) =
    "Direct dependency of "
      <> prettyPrint name

----------

instance Printer UsageError where
  prettyPrint (CouldNotResolveDeps names) =
    "Could not resolve deps: " <> T.intercalate ", " (prettyPrint <$> names)
  prettyPrint (CouldNotFindBinding name) =
    "Could not find binding " <> prettyPrint name
  prettyPrint (CouldNotFindStoreExpression exprHash) =
    "Could not find store expression for hash " <> prettyPrint exprHash
