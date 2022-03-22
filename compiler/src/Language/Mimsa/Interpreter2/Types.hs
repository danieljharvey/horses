module Language.Mimsa.Interpreter2.Types (InterpreterM, StackFrame (..), Stack (..)) where

import Control.Monad.Reader
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error.InterpreterError

type InterpreterM var ann a = ReaderT (Stack var ann) (Either (InterpreterError ann)) a

newtype StackFrame var ann = StackFrame
  { sfItems :: Map var (Expr var ann)
  }

newtype Stack var ann = Stack {getStack :: NE.NonEmpty (StackFrame var ann)}
