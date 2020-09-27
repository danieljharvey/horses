{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Mimsa.Backend.Javascript
  ( output,
    jsStandardLibrary,
    Javascript (..),
  )
where

import Data.Coerce
import Data.FileEmbed
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers

----
newtype Javascript = Javascript Text
  deriving (Eq, Ord, Show, Semigroup, Monoid, Printer)

instance IsString Javascript where
  fromString = Javascript . T.pack

----

-- these are saved in a file that is included in compilation
jsStandardLibrary :: Javascript
jsStandardLibrary =
  Javascript $ T.decodeUtf8 $(embedFile "static/backend/javascript/stdlib.js")

---

outputLiteral :: Literal -> Javascript
outputLiteral MyUnit = "{}"
outputLiteral (MyString s) = "\"" <> coerce s <> "\""
outputLiteral (MyBool True) = "true"
outputLiteral (MyBool False) = "false"
outputLiteral (MyInt i) = fromString $ show i

intercal :: Javascript -> [Javascript] -> Javascript
intercal sep as = Javascript $ T.intercalate (coerce sep) (coerce as)

outputRecord :: Map Name (Expr Name) -> Javascript
outputRecord as =
  "{ "
    <> intercal
      ", "
      ( outputRecordItem
          <$> M.toList as
      )
    <> " }"
  where
    outputRecordItem (name, val) =
      Javascript (prettyPrint name) <> ": " <> outputJS val

outputConsApp :: Expr Name -> Expr Name -> Javascript
outputConsApp c a = "__app(" <> outputJS c <> ", " <> outputJS a <> ")"

outputCaseMatch :: Expr Name -> NonEmpty (TyCon, Expr Name) -> Maybe (Expr Name) -> Javascript
outputCaseMatch value matches catchAll =
  "__match(" <> outputJS value <> ", " <> matchList <> ", " <> catcher <> ")"
  where
    matchList =
      "{ " <> intercal ", " (outputMatch <$> NE.toList matches) <> " }"
    outputMatch (tyCon, val) =
      Javascript (prettyPrint tyCon) <> ": " <> outputJS val
    catcher =
      maybe "null" outputJS catchAll

output :: Expr Name -> Javascript
output = outputJS

-- are there any more bindings in this expression?
containsLet :: Expr Name -> Bool
containsLet = getAny . foundLet

-- check for let expressions
foundLet :: Expr Name -> Any
foundLet (MyVar _) = mempty
foundLet (MyIf a b c) = foundLet a <> foundLet b <> foundLet c
foundLet MyLet {} = Any True
foundLet (MyLambda _ a) = foundLet a
foundLet (MyApp a b) = foundLet a <> foundLet b
foundLet (MyLiteral _) = mempty
foundLet MyLetPair {} = Any True
foundLet (MyPair a b) = foundLet a <> foundLet b
foundLet (MyRecord map') = foldMap foundLet map'
foundLet (MyRecordAccess a _) = foundLet a
foundLet (MyData _ a) =
  foundLet a
foundLet (MyConstructor _) = mempty
foundLet (MyConsApp a b) = foundLet a <> foundLet b
foundLet (MyCaseMatch sum' matches catchAll) =
  foundLet sum'
    <> mconcat (foundLet . snd <$> NE.toList matches)
    <> maybe mempty foundLet catchAll

-- if this is the last binding, then we should 'return' the statement
addReturn :: Expr Name -> Javascript -> Javascript
addReturn expr js = if not $ containsLet expr then "return " <> js else js

-- if a return contains let expresssions, it needs to be wrapped in curly lads
withCurlyBoys :: Expr Name -> Javascript -> Javascript
withCurlyBoys expr js = if containsLet expr then "{ " <> js <> " }" else js

outputJS :: Expr Name -> Javascript
outputJS expr =
  case expr of
    MyLiteral a ->
      outputLiteral a
    MyVar a -> coerce a
    MyLambda arg func -> coerce arg <> " => " <> withCurlyBoys func (outputJS func)
    MyApp f a -> outputJS f <> "(" <> outputJS a <> ")"
    MyIf p a b -> outputJS p <> " ? " <> outputJS a <> " : " <> outputJS b
    MyLet n a b ->
      "const " <> coerce n <> " = "
        <> outputJS a
        <> ";\n"
        <> addReturn b (outputJS b)
    MyRecord as -> outputRecord as
    MyLetPair m n a b ->
      "const [" <> coerce m <> "," <> coerce n <> "] = "
        <> outputJS a
        <> ";\n"
        <> addReturn b (outputJS b)
    MyPair a b -> "[" <> outputJS a <> "," <> outputJS b <> "]"
    MyRecordAccess r a -> outputJS r <> "." <> coerce a
    MyData _ a -> outputJS a -- don't output types
    MyConstructor a -> "{ type: \"" <> coerce a <> "\", vars: [] }"
    MyConsApp c a -> outputConsApp c a
    MyCaseMatch a matches catch -> outputCaseMatch a matches catch
