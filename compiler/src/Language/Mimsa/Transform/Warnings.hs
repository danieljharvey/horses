{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Transform.Warnings (Warning (..), getWarnings) where

import qualified Data.Set as S
import Language.Mimsa.Printer
import Language.Mimsa.Transform.FindUnused
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Store

-- | given an expression, generate a bunch of warnings for it like unused
-- variables etc
-- would be nice to warn about bad tail call stuff here too
newtype Warning
  = UnusedVariable Name
  deriving stock (Eq, Ord, Show)

instance Printer Warning where
  prettyPrint (UnusedVariable name) =
    "Unused variable: " <> prettyPrint name

getWarnings :: (Ord ann) => StoreExpression ann -> [Warning]
getWarnings se =
  let unused = findUnused (storeExpression se)
   in UnusedVariable . fst <$> S.toList unused
