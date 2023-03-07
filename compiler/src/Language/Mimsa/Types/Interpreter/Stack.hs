{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Interpreter.Stack (StackFrame (..), ExprData (..)) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import qualified Data.Text as T
import Language.Mimsa.Core
import Language.Mimsa.Types.Typechecker.Unique

data StackFrame var ann = StackFrame
  { sfVariables :: HashMap var (Expr (var, Unique) (ExprData var ann)),
    sfInfix :: HashMap InfixOp (Expr (var, Unique) (ExprData var ann))
  }
  deriving stock (Eq, Ord, Show)

instance (Hashable var) => Semigroup (StackFrame var ann) where
  (StackFrame varA infixA) <> (StackFrame varB infixB) =
    StackFrame (varA <> varB) (infixA <> infixB)

instance (Hashable var) => Monoid (StackFrame var ann) where
  mempty = StackFrame mempty mempty

instance (Printer var) => Printer (StackFrame var ann) where
  prettyPrint (StackFrame sfVars sfInf) =
    "{ vars: ["
      <> T.intercalate "," (prettyPrint <$> HM.keys sfVars)
      <> "], infix: ["
      <> T.intercalate "," (prettyPrint <$> HM.keys sfInf)
      <> "] }"

-- carried around in each node when interpreting
data ExprData var ann = ExprData
  { edStackFrame :: StackFrame var ann,
    edIsRecursive :: Bool,
    edAnnotation :: ann
  }
  deriving stock (Eq, Ord, Show)

instance (Hashable var, Semigroup ann) => Semigroup (ExprData var ann) where
  (ExprData sfA isRecA annA) <> (ExprData sfB isRecB annB) = ExprData (sfA <> sfB) (isRecA || isRecB) (annA <> annB)

instance (Hashable var, Monoid ann) => Monoid (ExprData var ann) where
  mempty = ExprData mempty False mempty
