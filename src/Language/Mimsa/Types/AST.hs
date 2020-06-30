{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.AST
  ( Expr (..),
    Literal (..),
    MonoType (..),
    FuncName (..),
    SumSide (..),
    StringType (..),
    UniVar (..),
    ForeignFunc (..),
    Scheme (..),
  )
where

import qualified Data.Aeson as JSON
import Data.List.NonEmpty
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics
import Language.Mimsa.Types.Name

------------

newtype StringType = StringType Text
  deriving newtype (Eq, Ord, Show, JSON.FromJSON, JSON.ToJSON)

newtype UniVar = UniVar Int
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, Num)

newtype FuncName = FuncName Text
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, JSON.FromJSON, JSON.ToJSON)

data Literal
  = MyInt Int
  | MyBool Bool
  | MyString StringType
  | MyUnit
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, JSON.ToJSON)

data SumSide = MyLeft | MyRight
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, JSON.ToJSON)

data Expr
  = MyLiteral Literal
  | MyVar Name
  | MyLet Name Expr Expr -- binder, expr, body
  | MyLetPair Name Name Expr Expr -- binderA, binderB, expr, body
  | MyLetList Name Name Expr Expr -- binderHead, binderHead, expr, body
  | MyLambda Name Expr -- binder, body
  | MyApp Expr Expr -- function, argument
  | MyIf Expr Expr Expr -- expr, thencase, elsecase
  | MyCase Expr Expr Expr -- sumExpr, leftCase, rightCase
  | MyPair Expr Expr -- (a,b)
  | MySum SumSide Expr -- Left a | Right b
  | MyList (NonEmpty Expr) -- [a]
  | MyRecord (Map Name Expr) -- { dog: MyLiteral (MyInt 1), cat: MyLiteral (MyInt 2) }
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, JSON.ToJSON)

data MonoType
  = MTInt
  | MTString
  | MTBool
  | MTUnit
  | MTFunction MonoType MonoType -- argument, result
  | MTPair MonoType MonoType -- (a,b)
  | MTSum MonoType MonoType -- a | b
  | MTList MonoType -- [a]
  | MTRecord (Map Name MonoType) -- { foo: a, bar: b }
  | MTVar Name
  deriving (Eq, Ord, Show)

data Scheme = Scheme [Name] MonoType
  deriving (Eq, Ord, Show)

------

data ForeignFunc
  = NoArgs MonoType (IO Expr)
  | OneArg (MonoType, MonoType) (Expr -> IO Expr)
  | TwoArgs (MonoType, MonoType, MonoType) (Expr -> Expr -> IO Expr)
