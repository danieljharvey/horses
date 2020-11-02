{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Actions
  ( doReplAction,
    evaluateText,
    resolveStoreExpression,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Mimsa.Actions
import Language.Mimsa.Backend.Backend (Backend (..), goCompile)
import Language.Mimsa.Interpreter (interpret)
import Language.Mimsa.Printer
import Language.Mimsa.Project
  ( getCurrentBindings,
    getCurrentTypeBindings,
  )
import Language.Mimsa.Project.Versions
import Language.Mimsa.Repl.ExpressionBind
import Language.Mimsa.Repl.ExpressionWatch
import Language.Mimsa.Repl.Types
import Language.Mimsa.Store (createDepGraph)
import Language.Mimsa.Tui (goTui)
import Language.Mimsa.Types

doReplAction ::
  Project Annotation ->
  Text ->
  ReplAction Annotation ->
  IO (Project Annotation)
doReplAction env _ Help = do
  doHelp
  pure env
doReplAction env input ListBindings = do
  _ <- runReplM $ doListBindings env input
  pure env
doReplAction env _ Tui =
  goTui env
doReplAction env _ (Versions name) = do
  _ <- runReplM $ doVersions env name
  pure env
doReplAction env _ (Watch name) = do
  newEnv' <- runReplM $ doWatch env name
  case newEnv' of
    Just env' -> pure env'
    _ -> pure env
doReplAction env input (Evaluate expr) = do
  _ <- runReplM $ doEvaluate env input expr
  pure env
doReplAction env input (Tree expr) = do
  _ <- runReplM $ doTree env input expr
  pure env
doReplAction env input (Info expr) = do
  _ <- runReplM $ doInfo env input expr
  pure env
doReplAction env input (Bind name expr) = do
  newEnv <- runReplM $ doBind env input name expr
  pure (fromMaybe env newEnv)
doReplAction env input (BindType dt) = do
  newEnv <- runReplM $ doBindType env input dt
  pure (fromMaybe env newEnv)
doReplAction env input (OutputJS expr) = do
  _ <- runReplM (doOutputJS env input expr)
  pure env

----------

doHelp :: IO ()
doHelp = do
  T.putStrLn "~~~ MIMSA ~~~"
  T.putStrLn ":help - this help screen"
  T.putStrLn ":info <expr> - get the type of <expr>"
  T.putStrLn ":bind <name> = <expr> - binds <expr> to <name> and saves it in the environment"
  T.putStrLn ":bindType type Either a b = Left a | Right b - binds a new type and saves it in the environment"
  T.putStrLn ":list - show a list of current bindings in the environment"
  T.putStrLn ":outputJS <expr> - show JS code for <expr>"
  T.putStrLn ":tree <expr> - draw a dependency tree for <expr>"
  T.putStrLn ":versions <name> - list all versions of a binding"
  T.putStrLn ":watch <name> - put <name> into 'scratch.mimsa' and bind any changes"
  T.putStrLn ":tui - launch terminal user interface for exploring project"
  T.putStrLn "<expr> - Evaluate <expr>, returning it's simplified form and type"
  T.putStrLn ":quit - give up and leave"

----------

doInfo ::
  Project Annotation ->
  Text ->
  Expr Name Annotation ->
  ReplM Annotation ()
doInfo env input expr = do
  (ResolvedExpression type' _ _ _ _) <- liftRepl $ getTypecheckedStoreExpression input env expr
  replPrint $
    prettyPrint expr
      <> "/n:: "
      <> prettyPrint type'

----------

doTree ::
  Project Annotation ->
  Text ->
  Expr Name Annotation ->
  ReplM Annotation ()
doTree env input expr = do
  (ResolvedExpression _ storeExpr _ _ _) <- liftRepl $ getTypecheckedStoreExpression input env expr
  let graph = createDepGraph (mkName "expression") (store env) storeExpr
  replPrint graph

-------

doVersions :: Project Annotation -> Name -> ReplM Annotation ()
doVersions env name = do
  versions <- liftRepl $ findVersions env name
  let showIt (i, mt, expr', usages) = do
        replPrint $
          "#" <> T.pack (show i)
            <> ( if NE.length versions == i
                   then " (current)"
                   else ""
               )
        replPrint (expr', mt)
        if S.null usages
          then replPrint ("Dependency of 0 functions" :: Text)
          else
            replPrint $
              "Dependency of " <> (T.pack . show . S.size) usages
                <> " functions"
   in traverse_ showIt versions

------

doListBindings :: Project Annotation -> Text -> ReplM Annotation ()
doListBindings env input = do
  let showBind (name, StoreExpression expr _ _) =
        case getTypecheckedStoreExpression input env expr of
          Right (ResolvedExpression type' _ _ _ _) ->
            replPrint (prettyPrint name <> " :: " <> prettyPrint type')
          _ -> pure ()
  traverse_
    showBind
    ( getExprPairs
        (store env)
        (getCurrentBindings $ bindings env)
    )
  let showType dt = replPrint (prettyPrint dt)
  traverse_
    showType
    ( getTypesFromStore
        (store env)
        (getCurrentTypeBindings $ typeBindings env)
    )

----------

doEvaluate ::
  Project Annotation ->
  Text ->
  Expr Name Annotation ->
  ReplM Annotation ()
doEvaluate env input expr = do
  (ResolvedExpression type' _ expr' scope' swaps) <-
    liftRepl $ getTypecheckedStoreExpression input env expr
  simplified <- liftRepl (first InterpreterErr (interpret scope' swaps expr'))
  replPrint $
    prettyPrint simplified
      <> "\n::\n"
      <> prettyPrint type'

---------

doOutputJS ::
  Project Annotation ->
  Text ->
  Expr Name Annotation ->
  ReplM Annotation ()
doOutputJS env input expr = do
  (ResolvedExpression _ storeExpr' _ _ _) <-
    liftRepl $ getTypecheckedStoreExpression input env expr
  liftIO $ goCompile CommonJS (store env) storeExpr'
  replPrint ("Output to output/index.js" :: Text)
