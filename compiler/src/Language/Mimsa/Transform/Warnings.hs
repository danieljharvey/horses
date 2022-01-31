{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Transform.Warnings (Warning (..), getWarnings) where

import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Language.Mimsa.Printer
import Language.Mimsa.Transform.FindUnused
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Swaps

-- | given an expression, generate a bunch of warnings for it like unused
-- variables etc
-- would be nice to warn about bad tail call stuff here too
newtype Warning
  = UnusedVariable Name
  deriving stock (Eq, Ord, Show)

instance Printer Warning where
  prettyPrint (UnusedVariable name) =
    "Unused variable: " <> prettyPrint name

unusedWarning :: Swaps -> Variable -> Maybe Warning
unusedWarning swaps var =
  UnusedVariable <$> M.lookup var swaps

getWarnings :: (Ord ann) => ResolvedExpression ann -> [Warning]
getWarnings resolved =
  let unused = findUnused (reVarExpression resolved)
   in catMaybes $ unusedWarning (reSwaps resolved) . fst <$> S.toList unused
