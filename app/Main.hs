{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Lib

main :: IO ()
main = do
  repl []
  _ <- traverse (print . startInference) exprs
  pure ()

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
