{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Repl.Actions
  ( doReplAction,
  )
where

import Control.Monad (join)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Mimsa.Repl.Types
import Language.Mimsa.Store (saveExpr)
import Language.Mimsa.Syntax
import Language.Mimsa.Typechecker
import Language.Mimsa.Types

doReplAction :: StoreEnv -> ReplAction -> IO StoreEnv
doReplAction env (ListBindings) = do
  let showBind = \(name, expr) -> T.putStrLn $ case getType env expr of
        Right type' ->
          prettyPrint name <> " :: " <> prettyPrint type'
        _ -> ""
  _ <- traverse showBind (getExprPairs env)
  pure env
doReplAction env (Evaluate expr) = do
  case getType env expr of
    Left e' -> do
      print e'
      pure env
    Right type' -> do
      T.putStrLn $
        prettyPrint expr
          <> " :: "
          <> prettyPrint type'
      pure env
doReplAction env (Bind name expr) = do
  if M.member name (bindings env)
    then do
      T.putStrLn $ T.pack (show name) <> " is already bound"
      pure env
    else do
      case getType env expr of
        Left e' -> do
          print e'
          pure env
        Right type' -> do
          hash <- saveExpr expr
          T.putStrLn $
            "Bound " <> prettyPrint name <> " to " <> prettyPrint expr
              <> " :: "
              <> prettyPrint type'
          let newEnv = fromItem name expr hash
          pure (env <> newEnv)

getType :: StoreEnv -> Expr -> Either T.Text MonoType
getType env expr = startInference (chainExprs expr env)

getExprPairs :: StoreEnv -> [(Name, Expr)]
getExprPairs (StoreEnv items' bindings') = join $ do
  (name, hash) <- M.toList bindings'
  case M.lookup hash items' of
    Just item -> pure [(name, item)]
    _ -> pure []

chainExprs :: Expr -> StoreEnv -> Expr
chainExprs inner env =
  foldr (\(name, expr) a -> MyLet name expr a) inner (getExprPairs env)

fromItem :: Name -> Expr -> ExprHash -> StoreEnv
fromItem name expr hash = StoreEnv
  { items = M.singleton hash expr,
    bindings = M.singleton name hash
  }
