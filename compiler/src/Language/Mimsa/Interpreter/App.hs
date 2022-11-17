module Language.Mimsa.Interpreter.App (interpretApp) where

import Language.Mimsa.Core
import Language.Mimsa.Interpreter.Monad
import Debug.Trace
import Language.Mimsa.Interpreter.ToHOAS
import Language.Mimsa.Interpreter.Types
import Language.Mimsa.Types.AST.HOASExpr
import Language.Mimsa.Types.AST.Identifier
import Language.Mimsa.Printer

interpretApp ::
  (Eq ann,Show ann ) =>
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
      interpretFn (body intValue) >>= interpretFn
    thing@(MyRecursiveLambda _ _ident (Identifier _ recIdent) body) -> do
      -- interpret arg first
      intValue <- interpretFn value
      -- run the func
      result <- interpretFn (body intValue)
      traceShowM (prettyDoc (fromHOAS result))
      let result2 = replaceVars recIdent thing result
      traceShowM (prettyDoc (fromHOAS result2))

      -- replace all instances of the recursive identifier with the function
      interpretFn result2 >>= interpretFn

    (MyConstructor ann' modName const') ->
      MyApp ann (MyConstructor ann' modName const')
        <$> interpretFn value

    fn -> do
      -- try and resolve it into something we recognise
      intFn <- interpretFn fn

      if fromHOAS intFn == fromHOAS fn -- if it hasn't changed, we don't want to end up looping so give up and error
        then do
          intValue <- interpretFn value
          -- at least change the value
          pure (MyApp ann intFn intValue)
        else interpretFn (MyApp ann intFn value)
