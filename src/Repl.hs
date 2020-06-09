{-# LANGUAGE OverloadedStrings #-}

module Repl where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Infer
import Language
import Printer
import Store
import System.Console.Haskeline
import Types

repl :: IO ()
repl = runInputT defaultSettings (loop [])
  where
    loop :: [(Name, Expr)] -> InputT IO ()
    loop exprs' = do
      liftIO (putStrLn "")
      minput <- getInputLine ":> "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> do
          case parseExpr (T.pack input) of
            Left e -> do
              liftIO (print e)
              loop exprs'
            Right expr -> do
              let name = mkName $ "var" <> T.pack (show $ length exprs')
              case startInference (chainExprs expr exprs') of
                Left e' -> do
                  liftIO (print e')
                  loop exprs'
                Right type' -> do
                  (ExprHash hash) <- liftIO (saveExpr expr)
                  liftIO (print hash)
                  liftIO $ T.putStrLn $
                    prettyPrint name <> " | " <> prettyPrint expr
                      <> " :: "
                      <> prettyPrint type'
                  loop (exprs' <> [(name, expr)])

chainExprs :: Expr -> [(Name, Expr)] -> Expr
chainExprs inner exprs =
  foldr (\(name, expr) a -> MyLet name expr a) inner exprs
