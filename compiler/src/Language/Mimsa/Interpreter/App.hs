module Language.Mimsa.Interpreter.App (interpretApp) where

import Language.Mimsa.Core
import Language.Mimsa.Interpreter.Monad
import Debug.Trace
import Language.Mimsa.Interpreter.ToHOAS
import Language.Mimsa.Interpreter.Types
import qualified Language.Mimsa.Types.AST.HOASExpr as HOAS
import Language.Mimsa.Printer
import Language.Mimsa.Types.AST

interpretApp ::
  (Eq ann,Show ann ) =>
  InterpretFn ann ->
  ann ->
  InterpretExpr ann ->
  InterpretExpr ann ->
  InterpreterM ann (InterpretExpr ann)
interpretApp interpretFn ann myFn value =
  case myFn of
    (HOAS.MyLambda _ _ident body) -> do
      -- interpret arg first
      intValue <- interpretFn value
      -- run it
      interpretFn (body intValue) >>= interpretFn
    thing@(HOAS.MyRecursiveLambda _ _ident recIdent body) -> do
      -- interpret arg first
      intValue <- interpretFn value
      -- run the func
      result <- interpretFn (body intValue)

      traceShowM (prettyDoc (fromHOAS result))

      let withRecursiveFunc = toHOAS (MyLet ann recIdent (fromHOAS thing) (fromHOAS result))

      traceShowM (prettyDoc (fromHOAS withRecursiveFunc))

      interpretFn withRecursiveFunc >>= interpretFn

    (HOAS.MyConstructor ann' modName const') ->
      HOAS.MyApp ann (HOAS.MyConstructor ann' modName const')
        <$> interpretFn value

    fn -> do
      -- try and resolve it into something we recognise
      intFn <- interpretFn fn

      if fromHOAS intFn == fromHOAS fn -- if it hasn't changed, we don't want to end up looping so give up and error
        then do
          intValue <- interpretFn value
          -- at least change the value
          pure (HOAS.MyApp ann intFn intValue)
        else interpretFn (HOAS.MyApp ann intFn value)
