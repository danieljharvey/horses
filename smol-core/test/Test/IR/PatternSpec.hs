{-# LANGUAGE OverloadedStrings #-}

module Test.IR.PatternSpec (spec) where

import Control.Monad.Identity
import Control.Monad.State (evalState)
import Data.Bifunctor
import Data.Foldable (traverse_)
import qualified Data.List.NonEmpty as NE
import Smol.Core.ExprUtils
import Smol.Core.IR.FromExpr.Pattern (predicatesFromPattern)
import Smol.Core.IR.FromExpr.Types (FromExprState (..))
import Smol.Core.Typecheck.Elaborate (builtInTypes)
import Smol.Core.Types.GetPath (GetPath (StructPath, ValuePath))
import Smol.Core.Types.ParseDep
import qualified Smol.Core.Types.Pattern as Smol
  ( Pattern (PConstructor, PLiteral, PTuple, PWildcard),
  )
import Smol.Core.Types.PatternPredicate
  ( PatternPredicate (PathEquals),
  )
import Smol.Core.Types.Prim (Prim (PBool, PInt, PNat))
import Smol.Core.Types.Type (Type (TPrim), TypePrim (TPInt))
import Test.Helpers (tyBool, tyCons)
import Test.Hspec (Spec, describe, it, shouldBe)

env :: (Monoid ann) => FromExprState ann
env =
  FromExprState
    { dataTypes = builtInTypes Identity,
      freshInt = 0,
      fesModuleParts = mempty,
      vars = mempty
    }

parseDepToIdentity :: Type ParseDep ann -> Type Identity ann
parseDepToIdentity = mapTypeDep (\(ParseDep a _) -> Identity a)

spec :: Spec
spec = do
  describe "predicatesFromPattern" $ do
    let ty = TPrim () TPInt
    let testVals =
          first (fmap parseDepToIdentity)
            <$> [ (Smol.PWildcard ty, []),
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
          let predResult = evalState (predicatesFromPattern id input) env
          predResult `shouldBe` result
      )
      testVals
