module Test.Helpers where

import Data.Text (Text)
import Language.Mimsa.Types

bool :: (Monoid ann) => Bool -> Expr a ann
bool a = MyLiteral mempty (MyBool a)

int :: (Monoid ann) => Int -> Expr a ann
int a = MyLiteral mempty (MyInt a)

str :: (Monoid ann) => StringType -> Expr a ann
str a = MyLiteral mempty (MyString a)

str' :: (Monoid ann) => Text -> Expr a ann
str' = str . StringType

--
unknown :: (Monoid ann) => Int -> Type ann
unknown = MTVar mempty . NumberedVar

---

named :: Text -> Variable
named = NamedVar . Name

---

tvBound :: Int -> Variable
tvBound = NumberedVar

tvFree :: Int -> Variable
tvFree = NumberedVar
