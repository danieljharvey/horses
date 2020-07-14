{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Actions
  ( doReplAction,
    evaluateText,
  )
where

import Control.Monad (join)
import Data.Bifunctor (first)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Mimsa.Interpreter (interpret)
import Language.Mimsa.Repl.Types
import Language.Mimsa.Repl.Watcher
import Language.Mimsa.Store
  ( createDepGraph,
    createStoreExpression,
    saveExpr,
    substitute,
  )
import Language.Mimsa.Syntax (parseExpr)
import Language.Mimsa.Typechecker
import Language.Mimsa.Types

doReplAction :: StoreEnv -> ReplAction -> IO StoreEnv
doReplAction env Help = do
  T.putStrLn "~~~ MIMSA ~~~"
  T.putStrLn ":help - this help screen"
  T.putStrLn ":info <expr> - get the type of <expr>"
  T.putStrLn ":bind <name> = <expr> - binds <expr> to <name> and saves it in the environment"
  T.putStrLn ":list - show a list of current bindings in the environment"
  T.putStrLn ":tree <expr> - draw a dependency tree for <expr>"
  T.putStrLn "<expr> - Evaluate <expr>, returning it's simplified form and type"
  T.putStrLn ":quit - give up and leave"
  pure env
doReplAction env (ListBindings) = do
  let showBind = \(name, (StoreExpression _ expr)) -> T.putStrLn $ case getTypecheckedStoreExpression env expr of
        Right (type', _, _, _) ->
          prettyPrint name <> " :: " <> prettyPrint type'
        _ -> ""
  _ <- traverse showBind (getExprPairs (store env) (bindings env))
  pure env
doReplAction env Watch = do
  watchFile
    "./store/"
    ( do
        text <- T.readFile "./store/scratch.mimsa"
        T.putStrLn "scratch.mimsa updated!"
        case parseExpr (T.strip text) of
          Right expr -> do
            case getTypecheckedStoreExpression env expr of
              Left e' -> do
                T.putStrLn $ prettyPrint e'
              Right (type', storeExpr', expr', scope') -> do
                simplified <- interpret scope' expr'
                case simplified of
                  Left e -> do
                    T.putStrLn (prettyPrint e)
                  Right simplified' -> do
                    T.putStrLn $
                      "+ Using the following from scope: "
                        <> prettyPrint (storeBindings storeExpr')
                    T.putStrLn $
                      prettyPrint simplified'
                        <> " :: "
                        <> prettyPrint type'
          Left e -> do
            T.putStrLn e
    )
  pure env
doReplAction env (Evaluate expr) = do
  case getTypecheckedStoreExpression env expr of
    Left e' -> do
      T.putStrLn $ prettyPrint e'
      pure env
    Right (type', _, expr', scope') -> do
      simplified <- interpret scope' expr'
      case simplified of
        Left e -> do
          T.putStrLn (prettyPrint e)
          pure env
        Right simplified' -> do
          T.putStrLn $
            prettyPrint simplified'
              <> " :: "
              <> prettyPrint type'
          pure env
doReplAction env (Tree expr) = do
  case getTypecheckedStoreExpression env expr of
    Left e' -> do
      T.putStrLn (prettyPrint e')
      pure env
    Right (_, storeExpr, _, _) -> do
      let graph = createDepGraph (mkName "expression") (store env) storeExpr
      T.putStrLn (prettyPrint graph)
      pure env
doReplAction env (Info expr) = do
  case getTypecheckedStoreExpression env expr of
    Left e' -> do
      T.putStrLn $ prettyPrint e'
      pure env
    Right (type', _, _, _) -> do
      T.putStrLn $
        prettyPrint expr
          <> " :: "
          <> prettyPrint type'
      pure env
doReplAction env (Bind name expr) = do
  if M.member name (getBindings $ bindings env)
    then do
      T.putStrLn $ T.pack (show name) <> " is already bound"
      pure env
    else do
      case getTypecheckedStoreExpression env expr of
        Left e' -> do
          T.putStrLn (prettyPrint e')
          pure env
        Right (type', storeExpr, _, _) -> do
          hash <- saveExpr storeExpr
          T.putStrLn $
            "Bound " <> prettyPrint name <> " to " <> prettyPrint expr
              <> " :: "
              <> prettyPrint type'
          let newEnv = fromItem name storeExpr hash
          pure (env <> newEnv)

----------

getType :: Swaps -> Scope -> (Expr Variable) -> Either Error MonoType
getType swaps scope' expr =
  first TypeErr $ startInference swaps (chainExprs expr scope')

getExprPairs :: Store -> Bindings -> [(Name, StoreExpression)]
getExprPairs (Store items') (Bindings bindings') = join $ do
  (name, hash) <- M.toList bindings'
  case M.lookup hash items' of
    Just item -> pure [(name, item)]
    _ -> pure []

chainExprs ::
  Expr Variable ->
  Scope ->
  Expr Variable
chainExprs expr scope = finalExpr
  where
    finalExpr =
      foldl
        (\a (name, expr') -> MyLet name expr' a)
        expr
        (M.toList . getScope $ scope)

fromItem :: Name -> StoreExpression -> ExprHash -> StoreEnv
fromItem name expr hash =
  StoreEnv
    { store = Store $ M.singleton hash expr,
      bindings = Bindings $ M.singleton name hash
    }

getTypecheckedStoreExpression ::
  StoreEnv ->
  Expr Name ->
  Either Error (MonoType, StoreExpression, Expr Variable, Scope)
getTypecheckedStoreExpression env expr = do
  storeExpr <- first ResolverErr $ createStoreExpression (bindings env) expr
  let (swaps, newExpr, scope) = substitute (store env) storeExpr
  exprType <- getType swaps scope newExpr
  pure (exprType, storeExpr, newExpr, scope)

evaluateText :: StoreEnv -> Text -> Either Error (MonoType, Expr Variable, Scope)
evaluateText env input = do
  expr <- first OtherError $ parseExpr input
  (mt, _, expr', scope') <- getTypecheckedStoreExpression env expr
  pure (mt, expr', scope')
