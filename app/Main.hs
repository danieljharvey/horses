{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as T
import Lib

main :: IO ()
main = do
  repl
  _ <- traverse (print . startInference) exprs
  pure ()

repl :: IO ()
repl = do
  input <- T.getLine
  case parseExpr input of
    Left e -> print e
    Right expr -> case startInference expr of
      Left e' -> print e'
      Right type' -> do
        T.putStrLn $ (prettyPrint expr) <> " :: " <> prettyPrint type'
  repl

exprs :: [Expr]
exprs =
  [ MyInt 1,
    MyBool True,
    MyString (StringType "hello"),
    MyVar (mkName "x"),
    MyLet (mkName "x") (MyInt 42) (MyBool True),
    MyLet (mkName "x") (MyInt 42) (MyVar (mkName "x")),
    MyLet (mkName "x") (MyBool True) (MyLet (mkName "y") (MyInt 42) (MyVar (mkName "x"))),
    MyLet (mkName "x") (MyBool True) (MyLet (mkName "x") (MyInt 42) (MyVar (mkName "x"))),
    MyLambda (mkName "x") (MyBool True),
    identity,
    MyLambda (mkName "x") (MyLambda (mkName "y") (MyVar (mkName "x"))),
    MyApp (MyLambda (mkName "x") (MyBool True)) (MyInt 1),
    MyApp
      identity
      (MyInt 1),
    MyApp
      ( MyLambda
          (mkName "x")
          ( (MyIf (MyVar (mkName "x")) (MyInt 10) (MyInt 10))
          )
      )
      (MyInt 100),
    MyLambda (mkName "x") (MyApp (MyVar (mkName "x")) (MyVar (mkName "x")))
  ]

identity :: Expr
identity = (MyLambda (mkName "x") (MyVar (mkName "x")))
