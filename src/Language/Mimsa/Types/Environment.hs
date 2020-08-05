{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Environment where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import Language.Mimsa.Types.Printer
import Language.Mimsa.Types.Scheme
import Language.Mimsa.Types.Variable

---

newtype Environment = Environment {getEnvironment :: Map Variable Scheme}
  deriving (Eq, Ord, Show, Semigroup, Monoid)

instance Printer Environment where
  prettyPrint (Environment items) =
    "[\n"
      <> T.intercalate ", \n" (printRow <$> M.toList items)
      <> "\n]"
    where
      printRow (var, scheme) =
        prettyPrint var <> ": " <> prettyPrint scheme
---
