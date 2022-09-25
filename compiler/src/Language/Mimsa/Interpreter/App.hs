module Language.Mimsa.Interpreter.App (interpretApp) where

import Language.Mimsa.Core
import Language.Mimsa.Interpreter.Monad
import Language.Mimsa.Interpreter.Types
import Language.Mimsa.Types.AST.HOASExpr
import Language.Mimsa.Types.Interpreter.Stack
import Language.Mimsa.Interpreter.ToHOAS

interpretApp ::
  (Show ann, Show var) =>
  InterpretFn var ann ->
  ExprData ann ->
  InterpretExpr var ann ->
  InterpretExpr var ann ->
  InterpreterM var ann (InterpretExpr var ann)
interpretApp interpretFn ann myFn value =
  case myFn of
    (MyLambda (ExprData _ _) _ident body) -> do
      -- interpret arg first
      intValue <- interpretFn value
      -- run it 
      pure (body intValue)
    (MyConstructor ann' modName const') ->
      MyApp ann (MyConstructor ann' modName const')
        <$> interpretFn value
    fn -> do
      -- try and resolve it into something we recognise
      _intFn <- interpretFn fn

      --interpretFn (MyApp ann intFn value)
      error $ "is this where we go wrong? " <> show (fromHOAS fn)

        {-
      if intFn == fn -- if it hasn't changed, we don't want to end up looping so give up and error
        then do
          intValue <- interpretFn value
          -- at least change the value
          pure (MyApp ann intFn intValue)
        else interpretFn (MyApp ann intFn value) -}
