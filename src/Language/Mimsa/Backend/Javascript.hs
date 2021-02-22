{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Language.Mimsa.Backend.Javascript
  ( output,
    outputCommonJS,
    renderWithFunction,
    Javascript (..),
  )
where

import Control.Monad.Except
import Data.Coerce
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Backend.NormaliseConstructors
import Language.Mimsa.Backend.Shared
import Language.Mimsa.Backend.Types
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store

----
newtype Javascript = Javascript Text
  deriving (Eq, Ord, Show, Semigroup, Monoid, Printer)

instance IsString Javascript where
  fromString = Javascript . T.pack

----

outputLiteral :: Literal -> Javascript
outputLiteral (MyUnit _) = "{}"
outputLiteral (MyString s) = "\"" <> coerce s <> "\""
outputLiteral (MyBool True) = "true"
outputLiteral (MyBool False) = "false"
outputLiteral (MyInt i) = fromString $ show i

intercal :: Javascript -> [Javascript] -> Javascript
intercal sep as = Javascript $ T.intercalate (coerce sep) (coerce as)

outputRecord ::
  (Monoid ann) =>
  Map Name (Expr Name ann) ->
  BackendM ann Javascript
outputRecord as = do
  items <- traverse outputRecordItem (M.toList as)
  pure $
    "{ "
      <> intercal
        ", "
        items
      <> " }"
  where
    outputRecordItem (name, val) = do
      js <- outputJS val
      pure (Javascript (prettyPrint name) <> ": " <> js)

outputCaseMatch ::
  (Monoid ann) =>
  Expr Name ann ->
  NonEmpty (TyCon, Expr Name ann) ->
  Maybe (Expr Name ann) ->
  BackendM ann Javascript
outputCaseMatch value matches catchAll = do
  jsValue <- outputJS value
  let outputMatch (tyCon, val) =
        outputJS val
          >>= ( \matchVal ->
                  pure $ Javascript (prettyPrint tyCon) <> ": " <> matchVal
              )
  jsMatches <- traverse outputMatch (NE.toList matches)
  let matchList =
        "{ " <> intercal ", " jsMatches <> " }"
  catcher <- case catchAll of
    Just catch' -> outputJS catch'
    Nothing -> pure "null"
  pure $
    "__match(" <> jsValue <> ", "
      <> matchList
      <> ", "
      <> catcher
      <> ")"

output ::
  (Monoid ann) =>
  ResolvedTypeDeps ->
  Expr Name ann ->
  BackendM ann Javascript
output dataTypes expr' =
  normaliseConstructors dataTypes expr' >>= outputJS

-- are there any more bindings in this expression?
containsLet :: Expr Name ann -> Bool
containsLet = getAny . foundLet

-- check for let expressions
foundLet :: Expr Name ann -> Any
foundLet = withMonoid findLet
  where
    findLet MyLet {} = Any True
    findLet MyLetPair {} = Any True
    findLet _ = mempty

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
  BackendM ann Javascript
outputOperator operator a b = do
  jsA <- outputJS a
  jsB <- outputJS b
  case operator of
    Equals ->
      pure $ "__eq(" <> jsA <> ", " <> jsB <> ")"
    Add ->
      pure $ jsA <> " + " <> jsB
    Subtract ->
      pure $ jsA <> " - " <> jsB
    StringConcat ->
      pure $ jsA <> " + " <> jsB
    (Custom op) -> throwError (OutputtingCustomOperator op)

intercalate :: Javascript -> [Javascript] -> Javascript
intercalate split as = coerce $ T.intercalate (coerce split) (coerce as)

outputConstructor ::
  (Monoid ann) =>
  TyCon ->
  [Expr Name ann] ->
  BackendM ann Javascript
outputConstructor tc args = do
  jsArgs <- traverse outputJS args
  let vars = intercalate "," jsArgs
  pure $ "{ type: \"" <> coerce tc <> "\", vars: [" <> vars <> "] }"

outputConsApp ::
  (Monoid ann) =>
  Expr Name ann ->
  Expr Name ann ->
  BackendM ann Javascript
outputConsApp a b = do
  let expr' = MyConsApp mempty a b
  tyCon <- getNestedTyCons expr'
  let args = getConsArgList expr'
  outputConstructor tyCon args

outputLambda ::
  (Monoid ann) =>
  Name ->
  Expr Name ann ->
  BackendM ann Javascript
outputLambda arg func = do
  jsFunc <- outputJS func
  pure $
    coerce arg <> " => "
      <> withCurlyBoys func jsFunc

outputLet ::
  (Monoid ann) =>
  Name ->
  Expr Name ann ->
  Expr Name ann ->
  BackendM ann Javascript
outputLet n a b = do
  jsA <- outputJS a
  jsB <- outputJS b
  pure $
    "const " <> coerce n <> " = "
      <> jsA
      <> ";\n"
      <> addReturn b jsB

outputLetPair ::
  (Monoid ann) =>
  Name ->
  Name ->
  Expr Name ann ->
  Expr Name ann ->
  BackendM ann Javascript
outputLetPair m n a b = do
  jsA <- outputJS a
  jsB <- outputJS b
  pure $
    "const [" <> coerce m <> "," <> coerce n <> "] = "
      <> jsA
      <> ";\n"
      <> addReturn b jsB

outputApp ::
  (Monoid ann) =>
  Expr Name ann ->
  Expr Name ann ->
  BackendM ann Javascript
outputApp f a = do
  jsF <- outputJS f
  jsA <- outputJS a
  pure $ jsF <> "(" <> jsA <> ")"

outputIf ::
  (Monoid ann) =>
  Expr Name ann ->
  Expr Name ann ->
  Expr Name ann ->
  BackendM ann Javascript
outputIf p a b = do
  jsP <- outputJS p
  jsA <- outputJS a
  jsB <- outputJS b
  pure $ jsP <> " ? " <> jsA <> " : " <> jsB

outputPair ::
  (Monoid ann) =>
  Expr Name ann ->
  Expr Name ann ->
  BackendM ann Javascript
outputPair a b = do
  jsA <- outputJS a
  jsB <- outputJS b
  pure $ "[" <> jsA <> "," <> jsB <> "]"

outputJS ::
  forall ann.
  (Monoid ann) =>
  Expr Name ann ->
  BackendM ann Javascript
outputJS expr =
  case expr of
    MyLiteral _ a ->
      pure $ outputLiteral a
    MyVar _ a -> pure $ coerce a
    MyInfix _ op a b -> outputOperator op a b
    MyLambda _ arg func -> outputLambda arg func
    MyApp _ f a -> outputApp f a
    MyIf _ p a b -> outputIf p a b
    MyLet _ n a b -> outputLet n a b
    MyRecord _ as -> outputRecord as
    MyLetPair _ m n a b -> outputLetPair m n a b
    MyPair _ a b -> outputPair a b
    MyRecordAccess _ r a -> do
      jsR <- outputJS r
      pure $ jsR <> "." <> coerce a
    MyData _ _ a -> outputJS a -- don't output types
    MyConstructor _ a -> outputConstructor @ann a []
    MyConsApp _ c a -> outputConsApp c a
    MyCaseMatch _ a matches catch ->
      outputCaseMatch a matches catch
    MyTypedHole _ a -> throwError (OutputingTypedHole a)
    MyDefineInfix _ _ _ a -> outputJS a -- don't output infix definitions

renderWithFunction ::
  (Monoid ann) =>
  ResolvedTypeDeps ->
  Name ->
  Expr Name ann ->
  BackendM ann Javascript
renderWithFunction dataTypes name expr =
  if containsLet expr && not (startsWithLambda expr)
    then do
      dt <- output dataTypes expr
      pure $
        "const " <> coerce name <> " = function() { "
          <> dt
          <> " }();\n"
    else do
      dt <- output dataTypes expr
      pure $
        "const " <> coerce name <> " = "
          <> dt
          <> ";\n"

startsWithLambda :: Expr var ann -> Bool
startsWithLambda MyLambda {} = True
startsWithLambda _ = False

outputCommonJS ::
  (Monoid ann) =>
  ResolvedTypeDeps ->
  StoreExpression ann ->
  BackendM ann Javascript
outputCommonJS dataTypes =
  outputStoreExpression
    CommonJS
    Renderer
      { renderFunc = renderWithFunction dataTypes,
        renderImport = \be (name, hash') ->
          pure $
            Javascript $
              "const "
                <> coerce name
                <> " = require(\"./"
                <> moduleFilename be hash'
                <> "\").main;\n",
        renderExport = \be name -> pure $ Javascript $ outputExport be name,
        renderStdLib = \be ->
          let filename = stdLibFilename be
           in pure $ Javascript $ "const { __match, __eq } = require(\"./" <> filename <> "\");\n"
      }
