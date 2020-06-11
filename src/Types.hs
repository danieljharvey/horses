{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Types
  ( Expr (..),
    MonoType (..),
    Name,
    StringType (..),
    UniVar (..),
    ExprHash (..),
    mkName,
    safeMkName,
    getName,
    validName,
  )
where

import qualified Data.Aeson as JSON
import qualified Data.Char as Ch
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics

newtype Name = Name {getName' :: Text}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, JSON.FromJSON, JSON.FromJSONKey, JSON.ToJSON)

getName :: Name -> Text
getName (Name t) = t

validName :: Text -> Bool
validName a =
  T.length a > 0
    && T.filter (Ch.isAlphaNum) a == a
    && Ch.isDigit (T.head a) == False

mkName :: Text -> Name
mkName a =
  if validName a
    then Name a
    else error $ T.unpack $ "name fail for '" <> a <> "'"

safeMkName :: Text -> Maybe Name
safeMkName a =
  if validName a
    then Just (Name a)
    else Nothing

------------

newtype ExprHash = ExprHash Int
  deriving (Eq, Ord, Show)
  deriving newtype (JSON.FromJSON, JSON.ToJSON)

newtype StringType = StringType Text
  deriving newtype (Eq, Ord, Show, JSON.FromJSON, JSON.ToJSON)

newtype UniVar = UniVar Int
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, Num)

data Expr
  = MyInt Int
  | MyBool Bool
  | MyString StringType
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
----------
