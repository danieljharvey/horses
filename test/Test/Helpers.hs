{-# LANGUAGE OverloadedStrings #-}

module Test.Helpers where

import Data.Text (Text)
import Language.Mimsa.Types

bool :: Bool -> Expr a
bool a = MyLiteral (MyBool a)

int :: Int -> Expr a
int a = MyLiteral (MyInt a)

str :: StringType -> Expr a
str a = MyLiteral (MyString a)

str' :: Text -> Expr a
str' = str . StringType

--
unknown :: Int -> MonoType
unknown = MTVar . TVFree . UniVar

---

named :: Text -> Variable
named = NamedVar . Name

builtIn :: Text -> Variable
builtIn = BuiltIn . Name

---

tvBound :: Int -> TypeVar
tvBound = TVBound . UniVar

tvFree :: Int -> TypeVar
tvFree = TVFree . UniVar
