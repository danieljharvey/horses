{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.Interpreter.Stack (StackFrame (..), Stack (..), ExprData (..)) where

import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Store.ExprHash

data StackFrame var ann = StackFrame
  { sfVariables :: Map var (Expr (var, Maybe ExprHash) (ExprData var ann)),
    sfInfix :: Map InfixOp (Expr (var, Maybe ExprHash) (ExprData var ann))
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

newtype Stack var ann = Stack {getStack :: NE.NonEmpty (StackFrame var ann)}

data ExprData var ann = ExprData
  { edStackFrame :: StackFrame var ann,
    edIsRecursive :: Bool
  }
  deriving stock (Eq, Ord, Show)

instance (Ord var) => Semigroup (ExprData var ann) where
  (ExprData sfA isRecA) <> (ExprData sfB isRecB) = ExprData (sfA <> sfB) (isRecA || isRecB)

instance (Ord var) => Monoid (ExprData var ann) where
  mempty = ExprData mempty False
