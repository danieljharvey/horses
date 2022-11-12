{-# LANGUAGE DerivingStrategies #-}

module Language.Mimsa.Types.Interpreter.Stack (ExprData (..)) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Language.Mimsa.Core
import Language.Mimsa.Types.Typechecker.Unique

-- carried around in each node when interpreting
data ExprData ann = ExprData
  { edIsRecursive :: Bool,
    edAnnotation :: ann
  }
  deriving stock (Eq, Ord, Show)

instance (Semigroup ann) => Semigroup (ExprData ann) where
  (ExprData isRecA annA) <> (ExprData isRecB annB) =
    ExprData (isRecA || isRecB) (annA <> annB)

instance (Monoid ann) => Monoid (ExprData ann) where
  mempty = ExprData False mempty
