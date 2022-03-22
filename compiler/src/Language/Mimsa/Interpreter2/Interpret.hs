module Language.Mimsa.Interpreter2.Interpret (interpret) where

import Control.Monad.Reader
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Language.Mimsa.Interpreter2.Types
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error

mapTopFrame :: (StackFrame var ann -> StackFrame var ann) -> Stack var ann -> Stack var ann
mapTopFrame f (Stack stack) =
  let currentFrame = NE.head stack
      restOfStack = NE.tail stack
   in Stack (f currentFrame :| restOfStack)

addToStackFrame :: (Ord var) => var -> Expr var ann -> Stack var ann -> Stack var ann
addToStackFrame var expr =
  mapTopFrame (\(StackFrame entries) -> StackFrame (M.singleton var expr <> entries))

varFromIdent :: Identifier var ann -> var
varFromIdent _ = undefined

initialStack :: (Ord var) => Stack var ann
initialStack = Stack (NE.singleton (StackFrame mempty))

interpret :: (Ord var) => Expr var ann -> Either (InterpreterError ann) (Expr var ann)
interpret expr = runReaderT (interpretAction expr) initialStack

interpretAction :: Expr var ann -> InterpreterM var ann (Expr var ann)
interpretAction lit@(MyLiteral {}) = pure lit
interpretAction (MyLet _ ident expr body) = do
  -- calc expr body
  interpretedExpr <- interpretAction expr

  pure body
interpretAction other = pure other
