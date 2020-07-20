module Language.Mimsa.Typechecker.Quantified where

import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Types.AST

-- go through an Expression and return a list of quantified (polymorphic)
-- variables that need to be instantiated each time they are used

getQuantified :: (Ord a) => (Expr a) -> Set a
getQuantified (MyVar _) = mempty
getQuantified (MyLiteral _) = mempty
getQuantified (MyIf a b c) = getQuantified a <> getQuantified b <> getQuantified c
getQuantified (MyLet _ a b) = (getQuantified a <> getQuantified b)
getQuantified (MyLambda _ a) = (getQuantified a)
getQuantified (MyForAllLambda newVar a) = S.singleton newVar <> (getQuantified a)
getQuantified (MyApp a b) = getQuantified a <> getQuantified b
getQuantified (MyCase sum' l r) = getQuantified sum' <> getQuantified l <> getQuantified r
getQuantified (MyLetPair _ _ a b) = getQuantified a <> getQuantified b
getQuantified (MyLetList _ _ a b) = getQuantified a <> getQuantified b
getQuantified (MyPair a b) = getQuantified a <> getQuantified b
getQuantified (MySum _ a) = getQuantified a
getQuantified (MyList as) = foldMap getQuantified as
getQuantified (MyRecord map') = foldMap getQuantified map'
getQuantified (MyRecordAccess a _) = getQuantified a
