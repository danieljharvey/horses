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

tyBool :: (Monoid ann) => Type ann
tyBool = TPrim mempty TPBool

tyBoolLit :: (Monoid ann) => Bool -> Type ann
tyBoolLit = TLiteral mempty . TLBool

tyInt :: (Monoid ann) => Type ann
tyInt = TPrim mempty TPInt

tyIntLit :: (Monoid ann) => Integer -> Type ann
tyIntLit = TLiteral mempty . TLInt

tyNat :: (Monoid ann) => Type ann
tyNat = TPrim mempty TPNat

tyVar :: (Monoid ann) => Text -> Type ann
tyVar = TVar mempty . Identifier

tyUnknown :: (Monoid ann) => Integer -> Type ann
tyUnknown = TUnknown mempty

tyTuple :: (Monoid ann) => Type ann -> [Type ann] -> Type ann
tyTuple a as = TTuple mempty a (NE.fromList as)

tyUnion :: (Monoid ann) => Type ann -> Type ann -> Type ann
tyUnion = TUnion mempty

tyCons :: (Monoid ann) => TypeName -> [Type ann] -> Type ann
tyCons typeName = foldl' (TApp mempty) (TConstructor mempty typeName)

unit :: (Monoid ann) => Expr ann
unit = EPrim mempty PUnit

bool :: (Monoid ann) => Bool -> Expr ann
bool = EPrim mempty . PBool

int :: (Monoid ann) => Integer -> Expr ann
int = EPrim mempty . PInt

nat :: (Monoid ann) => Natural -> Expr ann
nat = EPrim mempty . PNat

var :: (Monoid ann) => Text -> Expr ann
var = EVar mempty . Identifier

tuple :: (Monoid ann) => Expr ann -> [Expr ann] -> Expr ann
tuple a as = ETuple mempty a (NE.fromList as)

constructor :: (Monoid ann) => Text -> Expr ann
constructor lbl = EConstructor mempty (Constructor lbl)

patternMatch ::
  (Monoid ann) =>
  Expr ann ->
  [(Pattern ann, Expr ann)] ->
  Expr ann
patternMatch expr pats =
  EPatternMatch mempty expr (NE.fromList pats)

------

unsafeParseExpr :: Text -> Expr ()
unsafeParseExpr input = case parseExprAndFormatError input of
  Right expr -> expr $> ()
  Left e -> error (show e)

unsafeParseType :: Text -> Type ()
unsafeParseType input = case parseTypeAndFormatError input of
  Right ty -> ty $> ()
  Left e -> error (show e)

-- | parse a typed expr, ie parse it and fill the type with crap
unsafeParseTypedExpr :: Text -> Expr (Type Annotation)
unsafeParseTypedExpr input = case parseExprAndFormatError input of
  Right expr -> expr $> TPrim mempty TPBool
  Left e -> error (show e)
