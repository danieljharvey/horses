module Language.Mimsa.Interpreter.App (interpretApp) where

import Debug.Trace
import Language.Mimsa.Core
import qualified Language.Mimsa.Interpreter.HOASExpr as HOAS
import Language.Mimsa.Interpreter.Monad
import Language.Mimsa.Interpreter.ToHOAS
import Language.Mimsa.Interpreter.Types

interpretApp ::
  (Eq ann, Show ann) =>
  InterpretFn ann ->
  ann ->
  InterpretExpr ann ->
  InterpretExpr ann ->
  InterpreterM ann (InterpretExpr ann)
interpretApp interpretFn ann func arg = do
  case func of
    (HOAS.MyLambda _ _ident body) -> do
      -- interpret arg first
      intArg <- interpretFn arg

      -- run it
      interpretFn (body intArg)
    (HOAS.MyRecursiveLambda _ ident (Identifier _ innerRecIdent) body) -> do
      traceShowM ident
      traceShowM innerRecIdent
      -- the trick is that we make 'arg' available in context
      -- so we can reuse it in recursion
      withVar
        innerRecIdent
        arg
        ( interpretFn (body arg)
            >>= interpretFn
        )
    (HOAS.MyConstructor ann' modName const') ->
      HOAS.MyApp ann (HOAS.MyConstructor ann' modName const')
        <$> interpretFn arg
    fn -> do
      -- try and resolve it into something we recognise
      intFn <- interpretFn fn

      if fromHOAS intFn == fromHOAS fn -- if it hasn't changed, we don't want to end up looping so give up and error
        then do
          intArg <- interpretFn arg
          -- at least change the arg
          pure (HOAS.MyApp ann intFn intArg)
        else interpretFn (HOAS.MyApp ann intFn arg)
