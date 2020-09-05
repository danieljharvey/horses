{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Printer where

-- all of the pretty printing will happen here
-- rather than in the types files
-- to keep those smaller

import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Language.Mimsa.Printer.MonoType
import Language.Mimsa.Types.Construct
import Language.Mimsa.Types.DataType
import Language.Mimsa.Types.Expr
import Language.Mimsa.Types.FuncName
import Language.Mimsa.Types.Literal
import Language.Mimsa.Types.MonoType
import Language.Mimsa.Types.Name
import Language.Mimsa.Types.StringType
import Language.Mimsa.Types.TypeName
import Language.Mimsa.Types.Variable

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
  prettyPrint = getName

----

instance Printer BiIds where
  prettyPrint NoId = "-"
  prettyPrint (OneId v1) = prettyPrint v1
  prettyPrint (TwoIds v1 v2) = T.intercalate ", " (prettyPrint <$> [v1, v2])
  prettyPrint (ThreeIds v1 v2 v3) = T.intercalate ", " (prettyPrint <$> [v1, v2, v3])

instance Printer Variable where
  prettyPrint (NamedVar n) = prettyPrint n
  prettyPrint (NumberedVar i) = "U" <> T.pack (show i)
  prettyPrint (BuiltIn n) = prettyPrint n
  prettyPrint (BuiltInActual n _) = prettyPrint n

----

instance Printer Construct where
  prettyPrint = getConstruct

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
