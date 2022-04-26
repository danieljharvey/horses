{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Interpreter.Stack (StackFrame (..), ExprData (..)) where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Typechecker.Unique

data StackFrame var ann = StackFrame
  { sfVariables :: Map var (Expr (var, Unique) (ExprData var ann)),
    sfInfix :: Map InfixOp (Expr (var, Unique) (ExprData var ann))
  }
  deriving stock (Eq, Ord, Show)

instance (Ord var) => Semigroup (StackFrame var ann) where
  (StackFrame varA infixA) <> (StackFrame varB infixB) =
    StackFrame (varA <> varB) (infixA <> infixB)

instance (Ord var) => Monoid (StackFrame var ann) where
  mempty = StackFrame mempty mempty

instance (Printer var) => Printer (StackFrame var ann) where
  prettyPrint (StackFrame sfVars sfInf) =
    "{ vars: [" <> T.intercalate "," (prettyPrint <$> M.keys sfVars) <> "], infix: ["
      <> T.intercalate "," (prettyPrint <$> M.keys sfInf)
      <> "] }"

-- carried around in each node when interpreting
data ExprData var ann = ExprData
  { edStackFrame :: StackFrame var ann,
    edIsRecursive :: Bool,
    edAnnotation :: ann
  }
  deriving stock (Eq, Ord, Show)

instance (Ord var, Semigroup ann) => Semigroup (ExprData var ann) where
  (ExprData sfA isRecA annA) <> (ExprData sfB isRecB annB) = ExprData (sfA <> sfB) (isRecA || isRecB) (annA <> annB)

instance (Ord var, Monoid ann) => Monoid (ExprData var ann) where
  mempty = ExprData mempty False mempty
