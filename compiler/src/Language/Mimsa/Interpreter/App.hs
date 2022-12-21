module Language.Mimsa.Interpreter.App (interpretApp) where

import Language.Mimsa.Core
import Language.Mimsa.Interpreter.Monad
import Debug.Trace
import Language.Mimsa.Logging
import Language.Mimsa.Interpreter.ToHOAS
import Language.Mimsa.Interpreter.Types
import qualified Language.Mimsa.Types.AST.HOASExpr as HOAS

interpretApp ::
  (Eq ann ) =>
  InterpretFn ann ->
  ann ->
  InterpretExpr ann ->
  InterpretExpr ann ->
  InterpreterM ann (InterpretExpr ann)
interpretApp interpretFn ann myFn value = do
  debugPrettyM "function" (fromHOAS myFn)
  case myFn of
    (HOAS.MyLambda _ ident body) -> do
      debugPrettyM "lambda" ident

      -- interpret arg first
      intValue <- interpretFn value
      -- run it
      interpretFn (body intValue) >>= interpretFn
    _recursiveFunc@(HOAS.MyRecursiveLambda _ ident recIdent body) -> do
      debugPrettyM "recursive lambda" ident

      -- interpret arg first
      intValue <- interpretFn value
      -- run the func
      result <- interpretFn (body intValue)

      debugPrettyM "recursion result" (fromHOAS result)
      debugPrettyM "recursion identifier" recIdent

      let withRecursiveFunc = HOAS.MyApp ann result (HOAS.MyLambda ann recIdent body)
      debugPrettyM "recursion func to push" (fromHOAS withRecursiveFunc)

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
