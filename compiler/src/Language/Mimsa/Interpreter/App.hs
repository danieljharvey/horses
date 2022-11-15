module Language.Mimsa.Interpreter.App (interpretApp) where

import Language.Mimsa.Core
import Language.Mimsa.Interpreter.Monad
import Debug.Trace
import Language.Mimsa.Interpreter.ToHOAS
import Language.Mimsa.Interpreter.Types
import Language.Mimsa.Types.AST.HOASExpr

interpretApp ::
  (Eq ann,Show ann) =>
  InterpretFn ann ->
  ann ->
  InterpretExpr ann ->
  InterpretExpr ann ->
  InterpreterM ann (InterpretExpr ann)
interpretApp interpretFn ann myFn value =
  case myFn of
    (MyLambda _ _ident body) -> do
      -- interpret arg first
      intValue <- interpretFn value
      -- run it
      interpretFn (body intValue)
    thing@(MyRecursiveLambda _ _ident body) -> do
      -- interpret arg first
      intValue <- interpretFn value
      -- run it twice because of potential recursion
      interpretFn (body thing intValue) >>= interpretFn

    (MyConstructor ann' modName const') ->
      MyApp ann (MyConstructor ann' modName const')
        <$> interpretFn value

    fn -> do
      -- try and resolve it into something we recognise
      intFn <- interpretFn fn

      -- TODO: do we still need this safety shit
      if fromHOAS intFn == fromHOAS fn -- if it hasn't changed, we don't want to end up looping so give up and error
        then do
          intValue <- interpretFn value
          -- at least change the value
          pure (MyApp ann intFn intValue)
        else interpretFn (MyApp ann intFn value)
