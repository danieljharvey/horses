{-# LANGUAGE OverloadedStrings #-}

module Test.Helpers where

import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Types

bool :: Bool -> Expr
bool a = MyLiteral (MyBool a)

int :: Int -> Expr
int a = MyLiteral (MyInt a)

str :: StringType -> Expr
str a = MyLiteral (MyString a)

str' :: Text -> Expr
str' = str . StringType

--
unknown :: Int -> MonoType
unknown i = MTVar (mkName $ "U" <> (T.pack (show i)))
