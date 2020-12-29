{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Actions
  ( doReplAction,
    evaluateText,
    resolveStoreExpression,
  )
where

import Control.Monad.Reader
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
import Language.Mimsa.Repl.Types
import Language.Mimsa.Server.EnvVars
import Language.Mimsa.Store (createDepGraph)
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Error
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Store

doReplAction ::
  MimsaConfig ->
  Project Annotation ->
  Text ->
  ReplAction Annotation ->
  IO (Project Annotation)
doReplAction mimsaConfig env input action =
  case action of
    Help -> do
      doHelp
      pure env
    ListBindings -> do
      _ <- runReplM mimsaConfig $ doListBindings env input
      pure env
    (Versions name) -> do
      _ <- runReplM mimsaConfig $ doVersions env name
      pure env
    (Evaluate expr) -> do
      _ <- runReplM mimsaConfig $ doEvaluate env input expr
      pure env
    (Tree expr) -> do
      _ <- runReplM mimsaConfig $ doTree env input expr
      pure env
    (Info expr) -> do
      _ <- runReplM mimsaConfig $ doInfo env input expr
      pure env
    (Bind name expr) -> do
      newEnv <- runReplM mimsaConfig $ doBind env input name expr
      pure (fromMaybe env newEnv)
    (BindType dt) -> do
      newEnv <- runReplM mimsaConfig $ doBindType env input dt
      pure (fromMaybe env newEnv)
    (OutputJS expr) -> do
      _ <- runReplM mimsaConfig (doOutputJS env input expr)
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
  mimsaConfig <- ask
  outputPath <- liftIO $ goCompile mimsaConfig CommonJS (store env) storeExpr'
  replPrint ("Output to " <> outputPath)
