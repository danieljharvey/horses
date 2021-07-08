{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Language.Mimsa.Backend.Javascript
  ( output,
    outputCommonJS,
    outputPattern,
    renderWithFunction,
    Javascript (..),
  )
where

import Control.Monad.Except
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Coerce
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Data.String
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Language.Mimsa.Backend.NormaliseConstructors
import Language.Mimsa.Backend.Shared
import Language.Mimsa.Backend.Types
import Language.Mimsa.ExprUtils
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
  ( Expr (..),
    Literal (..),
    Operator (..),
    Pattern (..),
    Spread (..),
    StringPart (..),
    StringType (..),
  )
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store
import Language.Mimsa.Utils

----
newtype Javascript = Javascript LBS.ByteString
  deriving (Eq, Ord, Show, Semigroup, Monoid)

instance Printer Javascript where
  prettyPrint (Javascript bs) = (T.decodeUtf8 . B.concat . LBS.toChunks) bs

instance IsString Javascript where
  fromString = Javascript . LB.pack

----

textToJS :: Text -> Javascript
textToJS = Javascript . LB.fromChunks . return . T.encodeUtf8

outputLiteral :: Literal -> Javascript
outputLiteral (MyString s) = "\"" <> textToJS (coerce s) <> "\""
outputLiteral (MyBool True) = "true"
outputLiteral (MyBool False) = "false"
outputLiteral (MyInt i) = fromString $ show i

intToJS :: Int -> Javascript
intToJS = textToJS . prettyPrint

toPatternMap :: Javascript -> Pattern Name ann -> ([Guard], Map Name Javascript)
toPatternMap _ (PWildcard _) =
  (mempty, mempty)
toPatternMap name (PVar _ var) =
  (mempty, M.singleton var name)
toPatternMap name (PPair _ a b) =
  toPatternMap (name <> "[0]") a
    <> toPatternMap (name <> "[1]") b
toPatternMap name (PLit _ lit) =
  ([GuardEQ name (outputLiteral lit)], mempty)
toPatternMap name (PRecord _ items) =
  let subPattern (k, v) = toPatternMap (name <> "." <> textToJS (prettyPrint k)) v
   in mconcat (subPattern <$> M.toList items)
toPatternMap name (PConstructor _ tyCon args) =
  let tyConGuard = GuardEQ (name <> ".type") ("\"" <> textToJS (prettyPrint tyCon) <> "\"")
      subPattern i a = toPatternMap (name <> ".vars[" <> textToJS (prettyPrint (i - 1)) <> "]") a
   in ([tyConGuard], mempty) <> mconcat (mapWithIndex subPattern args)
toPatternMap name (PArray _ as spread) =
  let lengthGuard = case spread of
        NoSpread ->
          PrimEQ (name <> ".length") (intToJS . length $ as)
        (SpreadWildcard _) ->
          GreaterThanOrEQ (name <> ".length") (intToJS . length $ as)
        (SpreadValue _ _) ->
          GreaterThanOrEQ (name <> ".length") (intToJS . length $ as)
      subPattern i a = toPatternMap (name <> "[" <> textToJS (prettyPrint (i - 1)) <> "]") a
      spreadValue = case spread of
        SpreadValue _ a ->
          M.singleton a (name <> ".slice(" <> intToJS (length as) <> ")")
        _ -> mempty
   in ([lengthGuard], spreadValue) <> mconcat (mapWithIndex subPattern as)
toPatternMap name (PString _ a as) =
  let lengthGuard = GreaterThanOrEQ (name <> ".length") "1"
      aValue = case a of
        StrValue _ vA -> M.singleton vA (name <> ".charAt(0)")
        _ -> mempty
      asValue = case as of
        StrValue _ vAs -> M.singleton vAs (name <> ".slice(1)")
        _ -> mempty
   in ([lengthGuard], aValue <> asValue)

outputPattern :: Pattern Name ann -> Javascript
outputPattern pat =
  let (guards, vars) = toPatternMap "pat" pat
   in outputPatternMap vars guards

-- a pattern row is {...vars} => expr
outputPatternRow ::
  (Monoid ann) =>
  Pattern Name ann ->
  Expr Name ann ->
  BackendM ann Javascript
outputPatternRow pat expr = do
  js <- outputJS expr
  let (_, vars) = toPatternMap "" pat
  if null vars
    then pure ("() => " <> withBrackies js)
    else
      pure
        ( "({ "
            <> intercal
              ", "
              (textToJS . prettyPrint <$> M.keys vars)
            <> " }) => "
            <> withBrackies js
        )

data Guard
  = GuardEQ Javascript Javascript
  | PrimEQ Javascript Javascript
  | GreaterThanOrEQ Javascript Javascript

outputPatternMap :: Map Name Javascript -> [Guard] -> Javascript
outputPatternMap vars guards =
  "pat => "
    <> if null guards
      then "(" <> varJS <> ")"
      else guardsJS <> " ? " <> varJS <> " : null"
  where
    guardsJS =
      intercal " && " (showGuard <$> guards)
    showGuard (GuardEQ a b) =
      "__eq(" <> a <> ", " <> b <> ")"
    showGuard (PrimEQ a b) =
      a <> " === " <> b
    showGuard (GreaterThanOrEQ a b) =
      a <> " >= " <> b
    varJS =
      if M.null vars
        then "{}"
        else "{ " <> intercal ", " (showItem <$> M.toList vars) <> " }"

    showItem (k, v) = textToJS (prettyPrint k) <> ": " <> v

intercal :: Javascript -> [Javascript] -> Javascript
intercal sep as =
  Javascript $ LB.intercalate (coerce sep) (coerce as)

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
      pure (textToJS (prettyPrint name) <> ": " <> js)

outputArray ::
  (Monoid ann) =>
  [Expr Name ann] ->
  BackendM ann Javascript
outputArray as = do
  items <- traverse outputJS as
  pure $
    "["
      <> intercal
        ", "
        items
      <> "]"

outputPatternMatch ::
  (Monoid ann) =>
  Expr Name ann ->
  [(Pattern Name ann, Expr Name ann)] ->
  BackendM ann Javascript
outputPatternMatch expr patterns = do
  exprJS <- outputJS expr
  let outputPat (pat, patExpr) = do
        patRow <- outputPatternRow pat patExpr
        pure $ "[ " <> outputPattern pat <> ", " <> patRow <> " ]"
  pats <- traverse outputPat patterns
  let patternsJS = "[ " <> intercal ", " pats <> " ]"
  pure $ "__patternMatch(" <> exprJS <> ", " <> patternsJS <> ")"

output ::
  (Monoid ann) =>
  ResolvedTypeDeps ann ->
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
    findLet MyLet {} = (False, Any True) -- found one, stop looking
    findLet MyLetPair {} = (False, Any True) -- found one, stop looking
    findLet MyLetPattern {} = (False, Any True) -- found one, stop looking
    findLet MyLambda {} = (False, mempty) -- did not find one, but stop looking
    findLet _ = (True, mempty) -- did not find one, keep recursing

-- if this is the last binding, then we should 'return' the statement
addReturn :: Expr Name ann -> Javascript -> Javascript
addReturn expr js = if not $ containsLet expr then "return " <> js else js

-- if a return contains let expresssions, it needs to be wrapped in curly lads
withCurlyBoys :: Expr Name ann -> Javascript -> Javascript
withCurlyBoys expr js = if containsLet expr then "{ " <> js <> " }" else withBrackies js

withBrackies :: Javascript -> Javascript
withBrackies js =
  if LB.take 1 (coerce js) == "{"
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
    ArrayConcat ->
      pure $ "__concat(" <> jsA <> ", " <> jsB <> ")"
    (Custom op) -> throwError (OutputtingCustomOperator op)

intercalate :: Javascript -> [Javascript] -> Javascript
intercalate split as = coerce $ LB.intercalate (coerce split) (coerce as)

outputConstructor ::
  (Monoid ann) =>
  TyCon ->
  [Expr Name ann] ->
  BackendM ann Javascript
outputConstructor tc args = do
  jsArgs <- traverse outputJS args
  let vars = intercalate "," jsArgs
  pure $ "{ type: \"" <> textToJS (coerce tc) <> "\", vars: [" <> vars <> "] }"

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
    textToJS (coerce arg) <> " => "
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
    "const " <> textToJS (coerce n) <> " = "
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
    "const [" <> textToJS (coerce m) <> "," <> textToJS (coerce n) <> "] = "
      <> jsA
      <> ";\n"
      <> addReturn b jsB

outputLetPattern ::
  (Monoid ann) =>
  Pattern Name ann ->
  Expr Name ann ->
  Expr Name ann ->
  BackendM ann Javascript
outputLetPattern pat expr body = do
  patJS <- letPattern pat
  exprJS <- outputJS expr
  bodyJS <- outputJS body
  pure $ "const " <> patJS <> " = " <> exprJS <> ";\n" <> addReturn body bodyJS

letPattern :: Pattern Name ann -> BackendM ann Javascript
letPattern (PVar _ a) = pure $ textToJS (coerce a)
letPattern (PPair _ a b) = do
  pA <- letPattern a
  pB <- letPattern b
  pure ("[" <> pA <> ", " <> pB <> "]")
letPattern (PRecord _ as) = do
  pAs <- traverse letPattern as
  let items = (\(k, v) -> textToJS (coerce k) <> ": " <> v) <$> M.toList pAs
  pure $ "{ " <> intercal ", " items <> " }"
letPattern (PConstructor _ _ as) = do
  pAs <- traverse letPattern as
  pure $ "{ vars: [" <> intercal ", " pAs <> "] }"
letPattern (PWildcard _) = pure "_"
letPattern pat = throwError (OutputtingBadLetPattern pat)

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
    MyVar _ a -> pure $ textToJS (coerce a)
    MyInfix _ op a b -> outputOperator op a b
    MyLambda _ arg func -> outputLambda arg func
    MyApp _ f a -> outputApp f a
    MyIf _ p a b -> outputIf p a b
    MyLet _ n a b -> outputLet n a b
    MyLetPattern _ p e body -> outputLetPattern p e body
    MyRecord _ as -> outputRecord as
    MyArray _ as -> outputArray as
    MyLetPair _ m n a b -> outputLetPair m n a b
    MyPair _ a b -> outputPair a b
    MyRecordAccess _ r a -> do
      jsR <- outputJS r
      pure $ jsR <> "." <> textToJS (coerce a)
    MyData _ _ a -> outputJS a -- don't output types
    MyConstructor _ a -> outputConstructor @ann a []
    MyConsApp _ c a -> outputConsApp c a
    MyTypedHole _ a -> throwError (OutputingTypedHole a)
    MyDefineInfix _ _ _ a -> outputJS a -- don't output infix definitions
    MyPatternMatch _ tyCon args ->
      outputPatternMatch tyCon args

renderWithFunction ::
  (Monoid ann) =>
  ResolvedTypeDeps ann ->
  Name ->
  Expr Name ann ->
  BackendM ann Javascript
renderWithFunction dataTypes name expr =
  if containsLet expr && not (startsWithLambda expr)
    then do
      dt <- output dataTypes expr
      pure $
        "const " <> textToJS (coerce name) <> " = function() { "
          <> dt
          <> " }();\n"
    else do
      dt <- output dataTypes expr
      pure $
        "const " <> textToJS (coerce name) <> " = "
          <> dt
          <> ";\n"

startsWithLambda :: Expr var ann -> Bool
startsWithLambda MyLambda {} = True
startsWithLambda _ = False

outputCommonJS ::
  (Monoid ann) =>
  ResolvedTypeDeps ann ->
  StoreExpression ann ->
  BackendM ann Javascript
outputCommonJS dataTypes =
  outputStoreExpression
    CommonJS
    Renderer
      { renderFunc = renderWithFunction dataTypes,
        renderImport = \be (name, hash') ->
          pure $
            "const "
              <> textToJS (coerce name)
              <> " = require(\"./"
              <> Javascript (moduleFilename be hash')
              <> "\").main;\n",
        renderExport = \be name -> pure $ Javascript (outputExport be name),
        renderStdLib = \be ->
          let filename = Javascript (stdLibFilename be)
           in pure $ "const { __eq, __concat, __patternMatch } = require(\"./" <> filename <> "\");\n"
      }
