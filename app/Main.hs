module Main where

import Lib

main :: IO ()
main = do
  _ <- traverse (print . startInference) exprs
  pure ()

exprs :: [Expr]
exprs =
  [ MyInt 1,
    MyBool True,
    MyString "hello",
    MyVar (Name "x"),
    MyLet (Name "x") (MyInt 42) (MyBool True),
    MyLet (Name "x") (MyInt 42) (MyVar (Name "x")),
    MyLet (Name "x") (MyBool True) (MyLet (Name "y") (MyInt 42) (MyVar (Name "x"))),
    MyLet (Name "x") (MyBool True) (MyLet (Name "x") (MyInt 42) (MyVar (Name "x"))),
    MyLambda (Name "x") (MyBool True),
    identity,
    MyLambda (Name "x") (MyLambda (Name "y") (MyVar (Name "x"))),
    MyApp (MyLambda (Name "x") (MyBool True)) (MyInt 1),
    MyApp
      identity
      (MyInt 1),
    MyApp
      ( MyLambda
          (Name "x")
          ( (MyIf (MyVar (Name "x")) (MyInt 10) (MyInt 10))
          )
      )
      (MyInt 100),
    MyLambda (Name "x") (MyApp (MyVar (Name "x")) (MyVar (Name "x")))
  ]

identity :: Expr
identity = (MyLambda (Name "x") (MyVar (Name "x")))
