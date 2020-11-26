{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Language.Mimsa.Backend.Javascript
  ( output,
    commonJSStandardLibrary,
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
import Language.Mimsa.Backend.NormaliseConstructors
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store.ResolvedTypeDeps

----
newtype Javascript = Javascript Text
  deriving (Eq, Ord, Show, Semigroup, Monoid, Printer)

instance IsString Javascript where
  fromString = Javascript . T.pack

----

-- these are saved in a file that is included in compilation
commonJSStandardLibrary :: Javascript
commonJSStandardLibrary =
  Javascript $ T.decodeUtf8 $(embedFile "static/backend/commonjs/stdlib.js")

---

outputLiteral :: Literal -> Javascript
outputLiteral (MyUnit _) = "{}"
outputLiteral (MyString s) = "\"" <> coerce s <> "\""
outputLiteral (MyBool True) = "true"
outputLiteral (MyBool False) = "false"
outputLiteral (MyInt i) = fromString $ show i

intercal :: Javascript -> [Javascript] -> Javascript
intercal sep as = Javascript $ T.intercalate (coerce sep) (coerce as)

outputRecord :: (Monoid ann) => Map Name (Expr Name ann) -> Javascript
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

outputCaseMatch ::
  (Monoid ann) =>
  Expr Name ann ->
  NonEmpty (TyCon, Expr Name ann) ->
  Maybe (Expr Name ann) ->
  Javascript
outputCaseMatch value matches catchAll =
  "__match(" <> outputJS value <> ", " <> matchList <> ", " <> catcher <> ")"
  where
    matchList =
      "{ " <> intercal ", " (outputMatch <$> NE.toList matches) <> " }"
    outputMatch (tyCon, val) =
      Javascript (prettyPrint tyCon) <> ": " <> outputJS val
    catcher =
      maybe "null" outputJS catchAll

output :: (Monoid ann) => ResolvedTypeDeps -> Expr Name ann -> Javascript
output dataTypes = outputJS . normaliseConstructors dataTypes

-- are there any more bindings in this expression?
containsLet :: Expr Name ann -> Bool
containsLet = getAny . foundLet

-- check for let expressions
foundLet :: Expr Name ann -> Any
foundLet (MyVar _ _) = mempty
foundLet (MyIf _ a b c) = foundLet a <> foundLet b <> foundLet c
foundLet (MyInfix _ _ a b) = foundLet a <> foundLet b
foundLet MyLet {} = Any True
foundLet (MyLambda _ _ a) = foundLet a
foundLet (MyApp _ a b) = foundLet a <> foundLet b
foundLet (MyLiteral _ _) = mempty
foundLet MyLetPair {} = Any True
foundLet (MyPair _ a b) = foundLet a <> foundLet b
foundLet (MyRecord _ map') = foldMap foundLet map'
foundLet (MyRecordAccess _ a _) = foundLet a
foundLet (MyData _ _ a) =
  foundLet a
foundLet (MyConstructor _ _) = mempty
foundLet (MyConsApp _ a b) = foundLet a <> foundLet b
foundLet (MyCaseMatch _ sum' matches catchAll) =
  foundLet sum'
    <> mconcat (foundLet . snd <$> NE.toList matches)
    <> maybe mempty foundLet catchAll

-- if this is the last binding, then we should 'return' the statement
addReturn :: Expr Name ann -> Javascript -> Javascript
addReturn expr js = if not $ containsLet expr then "return " <> js else js

-- if a return contains let expresssions, it needs to be wrapped in curly lads
withCurlyBoys :: Expr Name ann -> Javascript -> Javascript
withCurlyBoys expr js = if containsLet expr then "{ " <> js <> " }" else withBrackies js

withBrackies :: Javascript -> Javascript
withBrackies js =
  if T.take 1 (coerce js) == "{"
    then "(" <> js <> ")"
    else js

outputOperator ::
  (Monoid ann) =>
  Operator ->
  Expr Name ann ->
  Expr Name ann ->
  Javascript
outputOperator Equals a b =
  "__eq(" <> outputJS a <> ", " <> outputJS b <> ")"
outputOperator Add a b = outputJS a <> " + " <> outputJS b
outputOperator Subtract a b = outputJS a <> " - " <> outputJS b
outputOperator StringConcat a b = outputJS a <> " + " <> outputJS b

intercalate :: Javascript -> [Javascript] -> Javascript
intercalate split as = coerce $ T.intercalate (coerce split) (coerce as)

outputConstructor :: (Monoid ann) => TyCon -> [Expr Name ann] -> Javascript
outputConstructor tc args =
  let vars = intercalate "," (outputJS <$> args)
   in "{ type: \"" <> coerce tc <> "\", vars: [" <> vars <> "] }"

outputConsApp ::
  (Monoid ann) =>
  Expr Name ann ->
  Expr Name ann ->
  Javascript
outputConsApp a b =
  let expr' = MyConsApp mempty a b
      tyCon = getNestedTyCons expr'
      args = getConsArgList expr'
   in outputConstructor tyCon args

outputJS ::
  forall ann.
  (Monoid ann) =>
  Expr Name ann ->
  Javascript
outputJS expr =
  case expr of
    MyLiteral _ a ->
      outputLiteral a
    MyVar _ a -> coerce a
    MyInfix _ op a b -> outputOperator op a b
    MyLambda _ arg func ->
      coerce arg <> " => " <> withCurlyBoys func (outputJS func)
    MyApp _ f a ->
      outputJS f <> "(" <> outputJS a <> ")"
    MyIf _ p a b ->
      outputJS p <> " ? " <> outputJS a <> " : " <> outputJS b
    MyLet _ n a b ->
      "const " <> coerce n <> " = "
        <> outputJS a
        <> ";\n"
        <> addReturn b (outputJS b)
    MyRecord _ as -> outputRecord as
    MyLetPair _ m n a b ->
      "const [" <> coerce m <> "," <> coerce n <> "] = "
        <> outputJS a
        <> ";\n"
        <> addReturn b (outputJS b)
    MyPair _ a b -> "[" <> outputJS a <> "," <> outputJS b <> "]"
    MyRecordAccess _ r a -> outputJS r <> "." <> coerce a
    MyData _ _ a -> outputJS a -- don't output types
    MyConstructor _ a -> outputConstructor @ann a []
    MyConsApp _ c a -> outputConsApp c a
    MyCaseMatch _ a matches catch -> outputCaseMatch a matches catch
