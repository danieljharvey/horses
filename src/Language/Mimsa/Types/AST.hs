{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Mimsa.Types.AST
  ( Expr (..),
    Literal (..),
    MonoType (..),
    StringType (..),
    UniVar (..),
  )
where

import qualified Data.Aeson as JSON
import Data.Text (Text)
import GHC.Generics
import Language.Mimsa.Types.Name

------------

newtype StringType = StringType Text
  deriving newtype (Eq, Ord, Show, JSON.FromJSON, JSON.ToJSON)

newtype UniVar = UniVar Int
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, Num)

data Literal
  = MyInt Int
  | MyBool Bool
  | MyString StringType
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, JSON.ToJSON)

data Expr
  = MyLiteral Literal
  | MyVar Name
  | MyLet Name Expr Expr -- binder, expr, body
  | MyLambda Name Expr -- binder, body
  | MyApp Expr Expr -- function, argument
  | MyIf Expr Expr Expr -- expr, thencase, elsecase
  deriving (Eq, Ord, Show, Generic, JSON.FromJSON, JSON.ToJSON)

data MonoType
  = MTInt
  | MTString
  | MTBool
  | MTFunction MonoType MonoType -- argument, result
  | MTUnknown (UniVar)
  deriving (Eq, Ord, Show)
