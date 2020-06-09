{-# LANGUAGE OverloadedStrings #-}

module Repl where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Infer
import Language
import Printer
import Types

repl :: [(Name, Expr)] -> IO ()
repl exprs' = do
  input <- T.getLine
  case parseExpr input of
    Left e -> do
      print e
      repl exprs'
    Right expr -> do
      let name = mkName $ "var" <> T.pack (show $ length exprs')
      case startInference (chainExprs expr exprs') of
        Left e' -> do
          print e'
          repl exprs'
        Right type' -> do
          T.putStrLn $
            prettyPrint name <> " | " <> prettyPrint expr
              <> " :: "
              <> prettyPrint type'
          repl (exprs' <> [(name, expr)])
