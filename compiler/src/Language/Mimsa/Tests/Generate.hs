module Language.Mimsa.Tests.Generate (generateFromMonoType) where

import qualified Data.Text as T
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker
import Test.QuickCheck

-- | TODO: this is wildly incomplete, but let's get the mechanism working first
fromMonoType :: (Monoid ann) => MonoType -> Gen (Expr Variable ann)
fromMonoType (MTPrim _ prim) =
  MyLiteral mempty <$> fromPrimitive prim
fromMonoType (MTArray _ mt) =
  MyArray mempty <$> listOf1 (fromMonoType mt) -- need to make this liftOf but tests can't handle empty list yet
fromMonoType (MTPair _ a b) =
  MyPair mempty <$> fromMonoType a <*> fromMonoType b
fromMonoType (MTRecord _ as) =
  MyRecord mempty <$> traverse fromMonoType as
fromMonoType _ = undefined

fromPrimitive :: Primitive -> Gen Literal
fromPrimitive MTBool =
  MyBool <$> chooseAny
fromPrimitive MTInt =
  MyInt <$> chooseAny
fromPrimitive MTString =
  -- TODO: are these valid StringType values? probably not, may be a beef when
  -- we come to Interpret these in tests
  MyString . StringType . T.pack <$> listOf chooseAny

generateFromMonoType :: (Monoid ann) => MonoType -> IO [Expr Variable ann]
generateFromMonoType mt = sample' (fromMonoType mt)