module Language.Mimsa.Properties.Generate (generateFromMonoType) where

import qualified Data.Text as T
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Typechecker
import Test.QuickCheck

fromMonoType :: MonoType -> Gen (Expr Variable ())
fromMonoType (MTPrim _ prim) = MyLiteral () <$> fromPrimitive prim
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

generateFromMonoType :: MonoType -> IO [Expr Variable ()]
generateFromMonoType mt = sample' (fromMonoType mt)
