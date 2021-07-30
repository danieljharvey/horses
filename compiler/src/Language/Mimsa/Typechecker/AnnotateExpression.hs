module Language.Mimsa.Typechecker.AnnotateExpression where

import Data.Functor
import Data.Monoid
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker

-- because of the way our typechecker works, we end up
-- with a single outcome monotype
-- however, perhaps we can use the substitutions generated on the way
-- to decorate the expression with types as a treat

newtype AnnotationWithType = AnnotationWithType
  { getAnnotationWithType ::
      ( Annotation,
        Maybe (Type Annotation)
      )
  }

instance Semigroup AnnotationWithType where
  (AnnotationWithType (a, tyA)) <> (AnnotationWithType (b, tyB)) =
    AnnotationWithType (a <> b, getFirst (First tyA <> First tyB))

instance Monoid AnnotationWithType where
  mempty = AnnotationWithType (mempty, Nothing)

-- our new expr type with all the nice types smashed into it
type TypeAnnotatedExpression =
  Expr Variable AnnotationWithType

-- | fmap the expression and try and catch as many useful types as possible
-- from the Substitutions
decorate :: Expr Variable Annotation -> TypeAnnotatedExpression
decorate expr = expr $> mempty
