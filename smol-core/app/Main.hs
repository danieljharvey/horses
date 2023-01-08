{-# LANGUAGE OverloadedStrings #-}

module Main where

import Compile.RunLLVM
import Control.Monad.Reader
import qualified Data.Text.IO as T
import Error.Diagnose
import qualified Interpreter.Convert as Interpret
import qualified Interpreter.Interpret as Interpret
import Parser
import Printer
import Typecheck
import Typecheck.Errors
import Types.Expr

doInterpret :: Expr ann -> Expr ()
doInterpret =
  Interpret.toExpr
    . flip runReader Interpret.emptyEnv
    . Interpret.interpret
    . Interpret.fromExpr
    . void

main :: IO ()
main = do
  putStrLn "hello smol"
  go

go :: IO ()
go = do
  input <- T.getLine
  case parseExprAndFormatError input of
    Left e -> print e
    Right expr -> do
      case elaborate expr of
        Left typeError -> do
          printDiagnostic stdout True True 2 defaultStyle (typeErrorDiagnostic input typeError)
        Right typedExpr -> do
          T.putStrLn "pretty: "
          T.putStrLn $ renderWithWidth 40 (prettyDoc typedExpr)
          let interpreted = doInterpret typedExpr
          T.putStrLn "interpreted: "
          T.putStrLn $ renderWithWidth 40 (prettyDoc interpreted)
          output <- run (moduleFromExpr typedExpr)
          T.putStrLn (rrResult output)
          T.putStrLn ("compilation: " <> rrComptime output)
          T.putStrLn ("runtime: " <> rrRuntime output)
  go
