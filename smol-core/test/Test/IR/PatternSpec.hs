{-# LANGUAGE OverloadedStrings #-}

module Test.IR.PatternSpec (spec) where

import Control.Monad.State
import Data.Foldable (traverse_)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import IR.FromExpr.Pattern
import IR.ToLLVM.Patterns
import Test.Helpers
import Test.Hspec
import Typecheck (builtInTypes)
import Types
import qualified Types as Smol

newtype Env ann = Env
  { dataTypes :: Map Smol.TypeName (Smol.DataType ann)
  }

emptyEnv :: (Monoid ann) => Env ann
emptyEnv =
  Env
    { dataTypes = builtInTypes
    }

spec :: Spec
spec = do
  describe "predicatesFromPattern" $ do
    let ty = TPrim () TPInt
    let testVals =
          [ (Smol.PWildcard ty, []),
            ( Smol.PLiteral ty (PBool True),
              [PathEquals ValuePath (PBool True)]
            ),
            ( Smol.PTuple
                ty
                (Smol.PLiteral ty (PBool True))
                ( NE.fromList
                    [Smol.PLiteral ty (PNat 1)]
                ),
              [ PathEquals (StructPath $ NE.singleton 0) (PBool True),
                PathEquals (StructPath $ NE.singleton 1) (PNat 1)
              ]
            ),
            ( Smol.PConstructor
                (tyCons "Maybe" [tyBool])
                "Just"
                [Smol.PLiteral ty (PBool True)],
              [ PathEquals (StructPath $ NE.singleton 0) (PInt 0),
                PathEquals (StructPath $ NE.singleton 1) (PBool True)
              ]
            ),
            ( Smol.PConstructor
                (tyCons "Maybe" [tyBool])
                "Nothing"
                mempty,
              [PathEquals (StructPath $ NE.singleton 0) (PInt 1)]
            )
          ]
    traverse_
      ( \(input, result) -> it (show input) $ do
          let predResult = evalState (predicatesFromPattern id input) emptyEnv
          predResult `shouldBe` result
      )
      testVals
