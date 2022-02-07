{-# LANGUAGE DerivingStrategies #-}

module Language.Mimsa.Backend.Core.TailCall
  ( isTailRecursive,
    listReturns,
    RecursionType (..),
    RecursionOccurrence (..),
  )
where

import qualified Data.Set as S
import Language.Mimsa.ExprUtils
import Language.Mimsa.Transform.FindUnused
import Language.Mimsa.Types.AST

-- nice

-- | does this let expression use recursion?
data RecursionType var ann
  = NoRecursion
  | Recursion
      { rootLetAnn :: ann,
        variableName :: var,
        recursionAnns :: [RecursionOccurrence ann]
      }
  deriving stock (Eq, Ord, Show)

-- | if so, is it the optimisable kind?
data RecursionOccurrence ann
  = TailRecursion ann
  | NonTailRecursion ann
  deriving stock (Eq, Ord, Show)

-- | detect what kind of recursion we are dealing with here
isTailRecursive ::
  (Ord var) =>
  Expr var ann ->
  var ->
  ann ->
  RecursionType var ann
isTailRecursive expr var ann =
  let exprsWithRecursion = filter (containsVar var) (listReturns expr)
      recType exp' =
        if isDirectRecursion var exp'
          then TailRecursion (getAnnotation exp')
          else NonTailRecursion (getAnnotation exp')
   in if null exprsWithRecursion
        then NoRecursion
        else Recursion ann var (recType <$> exprsWithRecursion)

-- | `fn 1 2` is direct, `1 + fn 2` is not
isDirectRecursion :: (Eq var) => var -> Expr var ann -> Bool
isDirectRecursion var (MyApp _ (MyVar _ var') _) | var == var' = True
isDirectRecursion var (MyApp _ first _) = isDirectRecursion var first
isDirectRecursion _ _ = False

containsVar :: (Ord var) => var -> Expr var ann -> Bool
containsVar var expr = S.member var (findUses expr)

listReturns :: Expr var ann -> [Expr var ann]
listReturns = withMonoid f
  where
    f (MyLet _ _ _ body) = (False, listReturns body)
    f (MyLetPattern _ _ _ body) = (False, listReturns body)
    f (MyData _ _ body) = (False, listReturns body)
    f (MyDefineInfix _ _ _ body) = (False, listReturns body)
    f (MyIf _ _ thenExpr elseExpr) = (False, listReturns thenExpr <> listReturns elseExpr)
    f (MyPatternMatch _ _ pats) = (False, mconcat (listReturns . snd <$> pats))
    f (MyLambda _ _ body) = (False, listReturns body)
    f other = (False, [other])
