{-# LANGUAGE OverloadedStrings #-}

module Test.IR.DataTypesSpec (spec, DataTypesState (..)) where

import Control.Monad.Identity
import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Smol.Core.IR.FromExpr.DataTypes as DT
import qualified Smol.Core.Typecheck as TC
import qualified Smol.Core.Typecheck.Types as Smol
import Smol.Core.Types
import qualified Smol.Core.Types as Smol
import Test.Helpers
import Test.Hspec

newtype DataTypesState ann = DataTypesState {dataTypes :: Map TypeName (DataType Identity ann)}

typeToDataType ::
  Smol.Type Identity () ->
  Either (Smol.TCError ()) DT.DataTypeInMemory
typeToDataType ty =
  evalState (DT.typeToDataTypeInMemory ty) (DataTypesState TC.builtInTypes)

spec :: Spec
spec = do
  describe "Data types in memory" $ do
    it "Enum shaped datatype" $ do
      let ty = tyCons "Ord" []
      typeToDataType ty
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
      typeToDataType ty
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
      typeToDataType ty
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
      typeToDataType ty
        `shouldBe` Right expected
