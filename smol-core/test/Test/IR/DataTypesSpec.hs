{-# LANGUAGE OverloadedStrings #-}

module Test.IR.DataTypesSpec (spec) where

import Control.Monad.Identity
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Smol.Core.IR.FromExpr.DataTypes as DT
import Smol.Core.IR.FromExpr.Types
import Smol.Core.IR.FromResolvedExpr
import Smol.Core.Typecheck.BuiltInTypes (builtInTypes)
import Smol.Core.Typecheck.FromParsedExpr
import qualified Smol.Core.Typecheck.Types as Smol
import Smol.Core.Types
import qualified Smol.Core.Types as Smol
import Test.Helpers
import Test.Hspec

typeToDataType ::
  Smol.Type Identity () ->
  Either (Smol.TCError ()) DT.DataTypeInMemory
typeToDataType ty =
  evalState
    (DT.typeToDataTypeInMemory ty)
    (FromExprState mempty (builtInTypes Identity) 0 mempty)

parseToIdentity :: Type ParseDep a -> Type Identity a
parseToIdentity = fromResolvedType . fromParsedType

spec :: Spec
spec = do
  describe "Data types in memory" $ do
    it "Enum shaped datatype" $ do
      let ty = tyCons "Ord" []
      typeToDataType (parseToIdentity ty)
        `shouldBe` Right DT.DTEnum

    it "Maybe Int" $ do
      let ty = tyCons "Maybe" [tyInt]
          expected =
            DT.DTDataType
              { DT.dtWhole = DT.DTTuple [DT.DTPrim TPInt, DT.DTArray 1 (DT.DTPrim TPInt)],
                DT.dtConstructors =
                  M.fromList
                    [ ("Just", [DT.DTPrim TPInt]),
                      ("Nothing", [])
                    ]
              }
      typeToDataType (parseToIdentity ty)
        `shouldBe` Right expected

    it "Either Int Bool" $ do
      let ty = tyCons "Either" [tyInt, tyBool]
          expected =
            DT.DTDataType
              { DT.dtWhole = DT.DTTuple [DT.DTPrim TPInt, DT.DTArray 1 (DT.DTPrim TPInt)],
                DT.dtConstructors =
                  M.fromList
                    [ ("Left", [DT.DTPrim TPInt]),
                      ("Right", [DT.DTPrim TPBool])
                    ]
              }
      typeToDataType (parseToIdentity ty)
        `shouldBe` Right expected

    it "These Int Bool" $ do
      let ty = tyCons "These" [tyInt, tyBool]
          expected =
            DT.DTDataType
              { DT.dtWhole = DT.DTTuple [DT.DTPrim TPInt, DT.DTArray 2 (DT.DTPrim TPInt)],
                DT.dtConstructors =
                  M.fromList
                    [ ("That", [DT.DTPrim TPBool]),
                      ("This", [DT.DTPrim TPInt]),
                      ("These", [DT.DTPrim TPInt, DT.DTPrim TPBool])
                    ]
              }
      typeToDataType (parseToIdentity ty)
        `shouldBe` Right expected
