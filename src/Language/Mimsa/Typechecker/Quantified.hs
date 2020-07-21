module Language.Mimsa.Typechecker.Quantified where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Language.Mimsa.Typechecker.TcMonad
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.MonoType
import Language.Mimsa.Types.Variable

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

freshQuantified :: Set Variable -> MonoType -> TcMonad ([Variable], MonoType)
freshQuantified vars mt = do
  newVars <- traverse (\_ -> NumberedVar <$> getNextUniVar) (S.toList vars)
  let fromTo = M.fromList (zip (S.toList vars) newVars)
  pure (newVars, swapTypes fromTo mt)

swapTypes :: Map Variable Variable -> MonoType -> MonoType
swapTypes fromTo (MTVar a) = MTVar $ fromMaybe (a) (M.lookup a fromTo)
swapTypes _ MTInt = MTInt
swapTypes _ MTString = MTString
swapTypes _ MTBool = MTBool
swapTypes _ MTUnit = MTUnit
swapTypes fromTo (MTFunction a b) = MTFunction (swapTypes fromTo a) (swapTypes fromTo b)
swapTypes fromTo (MTPair a b) = MTPair (swapTypes fromTo a) (swapTypes fromTo b)
swapTypes fromTo (MTSum a b) = MTSum (swapTypes fromTo a) (swapTypes fromTo b)
swapTypes fromTo (MTList a) = MTList (swapTypes fromTo a)
swapTypes fromTo (MTRecord as) = MTRecord (swapTypes fromTo <$> as)
