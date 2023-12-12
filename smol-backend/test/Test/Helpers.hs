module Test.Helpers
  ( tyBool,
    tyBoolLit,
    tyInt,
    tyIntLit,
    tyVar,
    tyUnknown,
    tyTuple,
    tyCons,
    bool,
    int,
    var,
    tuple,
    array,
    unit,
    identifier,
    constructor,
    patternMatch,
    unsafeParseExpr,
    unsafeParseModule,
    unsafeParseType,
    unsafeParseTypedExpr,
    fromParsedExpr,
    fromParsedType,
  )
where

import Data.Foldable (foldl')
import Data.Functor
import qualified Data.List.NonEmpty as NE
import qualified Data.Sequence as Seq
import qualified Data.Set.NonEmpty as NES
import Data.Text (Text)
import Smol.Core
import Smol.Modules.FromParts
import Smol.Modules.Parser
import Smol.Modules.Types.Module

resolve :: ParseDep a -> ResolvedDep a
resolve (ParseDep a _) = emptyResolvedDep a

-- | `ParsedExpr` has module names
-- | `ResolvedExpr` has module hashes and unique ids
-- this is like NumberVars from main `mimsa`, but for now we'll bodge it
-- to get things typechecking
fromParsedExpr :: ParsedExpr ann -> ResolvedExpr ann
fromParsedExpr = mapExprDep resolve

fromParsedType :: ParsedType ann -> ResolvedType ann
fromParsedType = mapTypeDep resolve

tyBool :: (Monoid ann) => Type dep ann
tyBool = TPrim mempty TPBool

tyBoolLit :: (Monoid ann) => Bool -> Type dep ann
tyBoolLit = TLiteral mempty . TLBool

tyInt :: (Monoid ann) => Type dep ann
tyInt = TPrim mempty TPInt

tyIntLit :: (Monoid ann) => [Integer] -> Type dep ann
tyIntLit = TLiteral mempty . TLInt . NES.fromList . NE.fromList

tyVar :: (Monoid ann) => Text -> Type ParseDep ann
tyVar = TVar mempty . emptyParseDep . Identifier

tyUnknown :: (Monoid ann) => Integer -> Type dep ann
tyUnknown = TUnknown mempty

tyTuple ::
  (Monoid ann) =>
  Type dep ann ->
  [Type dep ann] ->
  Type dep ann
tyTuple a as = TTuple mempty a (NE.fromList as)

tyCons ::
  (Monoid ann) =>
  TypeName ->
  [Type ParseDep ann] ->
  Type ParseDep ann
tyCons typeName =
  foldl' (TApp mempty) (TConstructor mempty (emptyParseDep typeName))

unit :: (Monoid ann) => Expr dep ann
unit = EPrim mempty PUnit

bool :: (Monoid ann) => Bool -> Expr dep ann
bool = EPrim mempty . PBool

int :: (Monoid ann) => Integer -> Expr dep ann
int = EPrim mempty . PInt

var :: (Monoid ann) => Text -> Expr ParseDep ann
var = EVar mempty . emptyParseDep . Identifier

tuple ::
  (Monoid ann) =>
  Expr dep ann ->
  [Expr dep ann] ->
  Expr dep ann
tuple a as = ETuple mempty a (NE.fromList as)

constructor ::
  (Monoid ann) =>
  Text ->
  Expr ParseDep ann
constructor lbl = EConstructor mempty (emptyParseDep (Constructor lbl))

identifier :: Text -> ParseDep Identifier
identifier = emptyParseDep . Identifier

patternMatch ::
  (Monoid ann) =>
  Expr dep ann ->
  [(Pattern dep ann, Expr dep ann)] ->
  Expr dep ann
patternMatch expr pats =
  EPatternMatch mempty expr (NE.fromList pats)

array :: (Monoid ann) => [Expr dep ann] -> Expr dep ann
array as = EArray mempty (Seq.fromList as)

------

unsafeParseExpr :: Text -> Expr ParseDep ()
unsafeParseExpr input = case parseExprAndFormatError input of
  Right expr -> expr $> ()
  Left e -> error (show e)

unsafeParseType :: Text -> Type ParseDep ()
unsafeParseType input = case parseTypeAndFormatError input of
  Right ty -> ty $> ()
  Left e -> error (show e)

unsafeParseModule :: Text -> Module ParseDep ()
unsafeParseModule input = case parseModuleAndFormatError input of
  Right parts -> case moduleFromModuleParts parts of
    Right a -> a $> ()
    Left e -> error (show e)
  Left e -> error (show e)

-- | parse a typed expr, ie parse it and fill the type with crap
-- this should not be compiled because `fromParsedExpr is nonsense
unsafeParseTypedExpr :: Text -> ResolvedExpr (Type ResolvedDep Annotation)
unsafeParseTypedExpr input = case parseExprAndFormatError input of
  Right expr -> fromParsedExpr expr $> TPrim mempty TPBool
  Left e -> error (show e)
