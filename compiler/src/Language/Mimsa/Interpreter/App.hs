module Language.Mimsa.Interpreter.App (interpretApp) where

import qualified Language.Mimsa.Interpreter.HOASExpr as HOAS
import Language.Mimsa.Interpreter.ToHOAS
import Language.Mimsa.Interpreter.Types
import Language.Mimsa.Logging
import Language.Mimsa.Core

interpretApp ::
  (Eq ann ,Show ann) =>
  InterpretFn ann ->
  ann ->
  InterpretExpr ann ->
  InterpretExpr ann ->
  InterpreterM ann (InterpretExpr ann)
interpretApp interpretFn ann func value = do
  case func of
    (HOAS.MyLambda _ ident body) -> do
      debugPrettyM "regular function" (fromHOAS func)
      debugPrettyM "value" (fromHOAS value)
      debugPrettyM "identifier" ident

      -- interpret arg first
      intValue <- interpretFn value

      -- run it
      interpretFn (body intValue) >>= interpretFn
    (HOAS.MyRecursiveLambda _ ident@(Identifier _ _innerIdent) recIdent@(Identifier _ _innerRecIdent) body) -> do
      debugPrettyM "recursive function" (fromHOAS func)
      debugPrettyM "value" (fromHOAS value)
      debugPrettyM "identifier" ident
      debugPrettyM "recursion identifier" recIdent
--thing@(HOAS.MyRecursiveLambda _ _ident recI@(Identifier _ recIdent) body)
--
      -- here `value` is the inner value
      -- interpret arg first
      intValue <- interpretFn value

      -- run the func
      let result = body intValue

      debugPrettyM "recursion result" (fromHOAS result)
      debugPrettyM "recursion identifier" recIdent
      --let withRecursiveFunc = toHOAS (MyLet ann recIdent (MyLambda lambdaAnn argIdent@(Identifier _ aIdent) lBody) rest)

      let withRecursiveFunc = toHOAS (MyLet ann recIdent (fromHOAS intValue) (fromHOAS result))


      --debugPrettyM "recursion func to push" withRecursiveFunc
      debugPrettyM "recursion func to push (munged)" (fromHOAS withRecursiveFunc)
      --debugPrettyM "recursion func to push new" (fromHOAS withRecursiveFunc2)

      pure withRecursiveFunc
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
