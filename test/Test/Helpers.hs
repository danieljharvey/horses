{-# LANGUAGE OverloadedStrings #-}

module Test.Helpers where

import Data.Text (Text)
import qualified Data.Text as T
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
unknown i = MTVar (mkName $ "U" <> (T.pack (show i)))

---

named :: Text -> Variable
named = NamedVar . Name
---
