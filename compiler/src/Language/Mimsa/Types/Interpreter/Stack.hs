{-# LANGUAGE DerivingStrategies #-}

module Language.Mimsa.Types.Interpreter.Stack (StackFrame (..), Stack (..)) where

import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import Language.Mimsa.Types.AST

data StackFrame var ann = StackFrame
  { sfVariables :: Map var (Expr var (StackFrame var ann)),
    sfInfix :: Map InfixOp (Expr var (StackFrame var ann))
  }
  deriving stock (Eq, Ord, Show)

instance (Ord var) => Semigroup (StackFrame var ann) where
  (StackFrame varA infixA) <> (StackFrame varB infixB) =
    StackFrame (varA <> varB) (infixA <> infixB)

instance (Ord var) => Monoid (StackFrame var ann) where
  mempty = StackFrame mempty mempty

newtype Stack var ann = Stack {getStack :: NE.NonEmpty (StackFrame var ann)}
