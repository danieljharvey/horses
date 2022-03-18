{-# LANGUAGE OverloadedStrings #-}

module Test.Typechecker.OutputTypes
  ( spec,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.Mimsa.Interpreter.UseSwaps as Swaps
import Language.Mimsa.Printer
import Language.Mimsa.Typechecker.OutputTypes
import Language.Mimsa.Types.AST
import Language.Mimsa.Types.Identifiers
import Language.Mimsa.Types.Project.SourceItem
import Language.Mimsa.Types.Project.SourceSpan
import Language.Mimsa.Types.ResolvedExpression
import Language.Mimsa.Types.Swaps
import Language.Mimsa.Types.Typechecker
import Test.Data.Project
import Test.Hspec
import Test.Utils.Helpers

unsafeTypecheckExpr ::
  Text ->
  ResolvedExpression Annotation
unsafeTypecheckExpr tx = case evaluateText testStdlib tx of
  Right a -> a
  Left e -> error (T.unpack (prettyPrint e))

useSwaps' :: Swaps -> Expr Variable ann -> Expr Name ann
useSwaps' swaps expr = case Swaps.useSwaps swaps expr of
  Right a -> a
  _ -> error "using swaps failed in OutputTypes test"

spec :: Spec
spec = do
  describe "Output types" $ do
    it "Single type in literal" $ do
      getExpressionSourceItems
        "True"
        ( MyLiteral
            (MTPrim (Location 1 4) MTBool)
            (MyBool True)
        )
        `shouldBe` [ SourceItem
                       "True :: Boolean"
                       ( SourceSpan
                           { ssRowStart = 1,
                             ssRowEnd = 1,
                             ssColStart = 2,
                             ssColEnd = 5
                           }
                       )
                   ]
    it "Gets location of lambda arg" $ do
      getExpressionSourceItems
        "\\a -> a"
        ( MyLambda
            (MTFunction (Location 0 7) (mtVar "a") (mtVar "a"))
            (Identifier (MTVar (Location 1 2) (tvNamed "a")) "a")
            ( MyVar (MTVar (Location 6 7) (tvNamed "a")) "a"
            )
        )
        `shouldBe` [ SourceItem "\\a -> a :: a -> a" (SourceSpan 1 1 1 8),
                     SourceItem "a :: a" (SourceSpan 1 1 2 3),
                     SourceItem "a :: a" (SourceSpan 1 1 7 8)
                   ]
    it "Includes pattern matches" $ do
      let expr =
            unsafeTypecheckExpr
              "\\a -> match a with (Just b) -> b | _ -> 0"

      let typedStoreExpr = useSwaps' (reSwaps expr) (reTypedExpression expr)
      getExpressionSourceItems (reInput expr) typedStoreExpr
        `shouldSatisfy` \a -> length a > 6
