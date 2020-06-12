{-# LANGUAGE OverloadedStrings #-}

module Repl where

import Control.Applicative
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Infer
import qualified Language as Mimsa
import qualified Parser as P
import Printer
import Store
  ( Environment (..),
    loadEnvironment,
    saveEnvironment,
    saveExpr,
  )
import System.Console.Haskeline
import Types

data ReplAction
  = Evaluate Expr
  | Bind Name Expr
  | ListBindings

replParser :: P.Parser ReplAction
replParser = evaluateParser <|> bindParser <|> listBindingsParser

evaluateParser :: P.Parser ReplAction
evaluateParser =
  Evaluate
    <$> P.right
      (P.thenSpace (P.literal ":info"))
      Mimsa.expressionParser

bindParser :: P.Parser ReplAction
bindParser =
  Bind
    <$> P.right (P.thenSpace (P.literal ":bind")) (P.thenSpace Mimsa.nameParser)
    <*> P.right (P.thenSpace (P.literal "=")) Mimsa.expressionParser

listBindingsParser :: P.Parser ReplAction
listBindingsParser = ListBindings <$ P.literal ":list"

fromItem :: Name -> Expr -> ExprHash -> Environment
fromItem name expr hash = Environment
  { items = M.singleton hash expr,
    bindings = M.singleton name hash
  }

repl :: IO ()
repl = do
  env <- loadEnvironment
  runInputT defaultSettings (loop (fromMaybe mempty env))
  where
    loop :: Environment -> InputT IO ()
    loop exprs' = do
      liftIO (putStrLn "")
      minput <- getInputLine ":> "
      case minput of
        Nothing -> return ()
        Just ":quit" -> return ()
        Just input -> do
          case P.runParserComplete replParser (T.pack input) of
            Left e -> do
              liftIO (print e)
              loop exprs'
            Right replAction -> do
              newExprs <- liftIO $ doReplAction exprs' replAction
              liftIO $ saveEnvironment newExprs
              loop newExprs

doReplAction :: Environment -> ReplAction -> IO Environment
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

getType :: Environment -> Expr -> Either T.Text MonoType
getType env expr = startInference (chainExprs expr env)

getExprPairs :: Environment -> [(Name, Expr)]
getExprPairs (Environment items' bindings') = join $ do
  (name, hash) <- M.toList bindings'
  case M.lookup hash items' of
    Just item -> pure [(name, item)]
    _ -> pure []

chainExprs :: Expr -> Environment -> Expr
chainExprs inner env =
  foldr (\(name, expr) a -> MyLet name expr a) inner (getExprPairs env)
