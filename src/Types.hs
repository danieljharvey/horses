{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types
  ( Expr (..),
    MonoType (..),
    Name (..),
    UniVar (..),
  )
where

import qualified Data.Aeson as JSON
import Data.Text (Text)
import GHC.Generics

newtype Name = Name {getName :: Text}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, JSON.FromJSON, JSON.ToJSON)

newtype UniVar = UniVar Int
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, Num)

data Expr
  = MyInt Int
  | MyBool Bool
  | MyString Text
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
