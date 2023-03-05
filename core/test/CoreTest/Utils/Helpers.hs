{-# LANGUAGE OverloadedStrings #-}

module CoreTest.Utils.Helpers
  ( joinLines,
    fromRight,
    fromLeft,
    fromJust,
    unsafeParseExpr',
    int,
    bool,
    unknown,
    str',
    textErrorContains,
    str,
    unsafeParseExpr,
    mtBool,
    mtString,
    mtVar,
    mtInt,
    unsafeParseDataType,
    tvNamed,
    tvNum,
    typeName,
    unsafeParseModuleItem,
    mtFun,
    unsafeParseMonoType,
  )
where

import Data.Functor
import Data.Text (Text)
import qualified Data.Text as T
import Language.Mimsa.Core

joinLines :: [Text] -> Text
joinLines = T.intercalate "\n"

fromRight :: (Printer e) => Either e a -> a
fromRight either' = case either' of
  Left e -> error (T.unpack $ prettyPrint e)
  Right a -> a

fromLeft :: Either e a -> e
fromLeft either' = case either' of
  Left e -> e
  Right _ -> error "Expected a Left!"

fromJust :: Maybe a -> a
fromJust maybe' = case maybe' of
  Just a -> a
  _ -> error "Expected a Just"

unsafeParseExpr' :: (Monoid ann) => Text -> Expr Name ann
unsafeParseExpr' t = case parseExpr t of
  Right a -> a $> mempty
  Left _ ->
    error $
      "Error parsing expr for Prettier tests:"
        <> T.unpack t

unsafeParseDataType :: Text -> DataType
unsafeParseDataType t = case parseTypeDecl t of
  Right a -> a
  Left _ -> error $ "could not parse data type: " <> T.unpack t

unsafeParseExpr :: Text -> Expr Name ()
unsafeParseExpr = unsafeParseExpr'

unsafeParseModuleItem :: (Monoid ann) => Text -> ModuleItem ann
unsafeParseModuleItem t = case parseAndFormat moduleParser t of
  Right [item] -> item $> mempty
  Right _many -> error "ModuleItem parser succeeded but did not have 1 item"
  Left e -> error $ "Error parsing ModuleItem for tests: " <> T.unpack (prettyPrint e)

unsafeParseMonoType :: Text -> Type ()
unsafeParseMonoType t = case parseMonoType t of
  Right a -> a $> ()
  Left _ ->
    error $
      "Error parsing monotype for Prettier tests:"
        <> T.unpack t

textErrorContains :: Text -> Either Text a -> Bool
textErrorContains s res = case res of
  Left e -> s `T.isInfixOf` e
  _ -> False

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
unknown = MTVar mempty . TVUnificationVar

typeName :: (Monoid ann) => Text -> Type ann
typeName = MTVar mempty . TVName . mkTyVar

---

tvNum :: Int -> TypeIdentifier
tvNum = TVUnificationVar

tvNamed :: Text -> TypeIdentifier
tvNamed t = TVName $ mkTyVar t

----

mtInt :: (Monoid ann) => Type ann
mtInt = MTPrim mempty MTInt

mtBool :: (Monoid ann) => Type ann
mtBool = MTPrim mempty MTBool

mtString :: (Monoid ann) => Type ann
mtString = MTPrim mempty MTString

mtVar :: (Monoid ann) => Text -> Type ann
mtVar n = MTVar mempty (tvNamed n)

mtFun :: (Monoid ann) => Type ann -> Type ann -> Type ann
mtFun = MTFunction mempty
