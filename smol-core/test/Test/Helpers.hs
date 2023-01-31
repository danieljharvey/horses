module Test.Helpers
  ( tyBool,
    tyBoolLit,
    tyInt,
    tyIntLit,
    tyNat,
    tyVar,
    tyUnknown,
    tyTuple,
    tyUnion,
    tyCons,
    bool,
    int,
    nat,
    var,
    tuple,
    unit,
    identifier,
    constructor,
    patternMatch,
    unsafeParseExpr,
    unsafeParseType,
    unsafeParseTypedExpr,
  )
where

import Data.Foldable (foldl')
import Data.Functor
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import GHC.Natural
import Smol.Core
import Smol.Core.Typecheck.FromParsedExpr

tyBool :: (Monoid ann) => Type dep ann
tyBool = TPrim mempty TPBool

tyBoolLit :: (Monoid ann) => Bool -> Type dep ann
tyBoolLit = TLiteral mempty . TLBool

tyInt :: (Monoid ann) => Type dep ann
tyInt = TPrim mempty TPInt

tyIntLit :: (Monoid ann) => Integer -> Type dep ann
tyIntLit = TLiteral mempty . TLInt

tyNat :: (Monoid ann) => Type dep ann
tyNat = TPrim mempty TPNat

tyVar :: (Monoid ann) => Text -> Type dep ann
tyVar = TVar mempty . Identifier

tyUnknown :: (Monoid ann) => Integer -> Type dep ann
tyUnknown = TUnknown mempty

tyTuple :: (Monoid ann) => Type dep ann -> [Type dep ann] -> Type dep ann
tyTuple a as = TTuple mempty a (NE.fromList as)

tyUnion :: (Monoid ann) => Type dep ann -> Type dep ann -> Type dep ann
tyUnion = TUnion mempty

tyCons :: (Monoid ann) => TypeName -> [Type dep ann] -> Type dep ann
tyCons typeName = foldl' (TApp mempty) (TConstructor mempty typeName)

unit :: (Monoid ann) => Expr dep ann
unit = EPrim mempty PUnit

bool :: (Monoid ann) => Bool -> Expr dep ann
bool = EPrim mempty . PBool

int :: (Monoid ann) => Integer -> Expr dep ann
int = EPrim mempty . PInt

nat :: (Monoid ann) => Natural -> Expr dep ann
nat = EPrim mempty . PNat

var :: (Monoid ann) => Text -> Expr ParseDep ann
var = EVar mempty . emptyParseDep . Identifier

tuple :: (Monoid ann) => Expr dep ann -> [Expr dep ann] -> Expr dep ann
tuple a as = ETuple mempty a (NE.fromList as)

constructor :: (Monoid ann) => Text -> Expr ParseDep ann
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

------

unsafeParseExpr :: Text -> Expr ParseDep ()
unsafeParseExpr input = case parseExprAndFormatError input of
  Right expr -> expr $> ()
  Left e -> error (show e)

unsafeParseType :: Text -> Type ParseDep ()
unsafeParseType input = case parseTypeAndFormatError input of
  Right ty -> ty $> ()
  Left e -> error (show e)

-- | parse a typed expr, ie parse it and fill the type with crap
unsafeParseTypedExpr :: Text -> ResolvedExpr (Type ResolvedDep Annotation)
unsafeParseTypedExpr input = case parseExprAndFormatError input of
  Right expr -> fromParsedExpr expr $> TPrim mempty TPBool
  Left e -> error (show e)
