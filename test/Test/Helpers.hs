module Test.Helpers where

import Data.Text (Text)
import Language.Mimsa.Types

bool :: (Monoid ann) => Bool -> Expr ann a
bool a = MyLiteral mempty (MyBool a)

int :: (Monoid ann) => Int -> Expr ann a
int a = MyLiteral mempty (MyInt a)

str :: (Monoid ann) => StringType -> Expr ann a
str a = MyLiteral mempty (MyString a)

str' :: (Monoid ann) => Text -> Expr ann a
str' = str . StringType

--
unknown :: Int -> MonoType
unknown = MTVar . NumberedVar

---

named :: Text -> Variable
named = NamedVar . Name

builtIn :: Text -> Variable
builtIn = BuiltIn . Name

---

tvBound :: Int -> Variable
tvBound = NumberedVar

tvFree :: Int -> Variable
tvFree = NumberedVar
